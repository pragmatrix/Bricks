﻿module BricksCore

(** BRICKS, F# COMPUTATION EXPRESSIONS AND COMBINATORS FOR LAZY INCREMENTAL COMPUTATION, SEE README.MD **)

open BrickDefs

open System
open System.Diagnostics
open System.Collections.Immutable
open System.Collections.Generic
open System.Linq

open InlineHelper

(** BRICK, ENVIRONMENT **)

type Brick =
    abstract member addReferrer : Brick -> unit
    abstract member removeReferrer : Brick -> unit
    abstract member invalidate : unit -> unit
    abstract member tryCollect : unit -> unit

[<AutoOpen>]
module BrickExtensions =
    type Brick with
        member this.removeReferrerAndTryCollect referrer =
            this.removeReferrer referrer
            this.tryCollect()

type internal Trace = Brick list

type internal ComputationResult<'v> = Trace * 'v

type internal Computation<'v> = 'v brick -> ComputationResult<'v>

and 't brick = Brick<'t>

and Brick<'v>(computation : Computation<'v>, ?initial: 'v) =
    
    let mutable _comp = computation
    let mutable _trace : Trace = []
    let mutable _value : 'v option = initial
    let mutable _alive = false
    let mutable _valid = false
    let mutable _referrer = ISet.empty

    member internal this.trace = _trace
    member internal this.valid = _valid
    member internal this.value = _value
    member internal this.instance : 'v option = if _alive then this.value else None

    interface Brick with
        member this.addReferrer r =
            _referrer <- _referrer.Add r

        member this.removeReferrer r =
            _referrer <- _referrer.Remove r

        member this.invalidate() =
            if _valid then
                _valid <- false
                _referrer |> Seq.iter (fun b -> b.invalidate())

        member this.tryCollect() =
            if (_valid && _referrer.Count = 0) then
                this.invalidate()
                match box _value.Value with
                | :? IDisposable as d -> d.Dispose()
                | _ -> ()
                _trace |> List.iter (fun dep -> dep.removeReferrerAndTryCollect this)

    member this.evaluate() : 'v = 
        if _valid then _value.Value else
        let t, c = _comp this
        // relink referrers (tbd: do this incrementally)
        _trace |> List.iter (fun dep -> dep.removeReferrer this)
        t |> List.iter (fun dep -> dep.addReferrer this)

        _trace |> List.iter (fun dep -> dep.tryCollect())

        _trace <- t
        _value <- Some c
        _alive <- true
        _valid <- true
        c

    member this.write v =
        this.invalidate()
        _comp <- fun _ -> [], v

    member this.reset() = 
        this.invalidate()
        _comp <- computation

    member this.invalidate() =
        (this :> Brick).invalidate()    

type 't bricks = seq<'t brick>

let internal makeBrick<'v> f = Brick<'v>(f)
let internal makeBrickInit<'v> initial f = Brick<'v>(f, initial)

type ManifestMarker<'a> = { instantiator: unit -> 'a }
    with 
        static member create f = { instantiator = f }

let inline manifest f = ManifestMarker<_>.create f

type BrickBuilder() =
    member this.Bind (dependency: 'dep brick, cont: 'dep -> Computation<'next>) : Computation<'next> =
        fun b ->
            let depValue = dependency.evaluate()
            let contDep, contValue = cont depValue b
            dependency:>Brick :: contDep, contValue

    member this.Bind (dependencies: 'dep brick seq, cont: 'dep seq -> Computation<'next>) : Computation<'next> =
        fun b ->
            let depValues = dependencies |> Seq.map (fun d -> d.evaluate())
            let contDep, contValue = cont depValues b
            let thisDeps = dependencies |> Seq.cast |> Seq.toList
            thisDeps @ contDep |> Seq.toList, contValue

    member this.Bind (manifest: ManifestMarker<'i>, cont: 'i -> Computation<'i>) : Computation<'i> =
        fun b ->
            let i = 
                match b.instance with
                | Some i -> i
                | None -> manifest.instantiator()
            cont i b

    member this.ReturnFrom (brick: 'value brick) = 
        fun _ ->
            let value = brick.evaluate();
            [brick:>Brick], value

    member this.Return value = 
        fun _ -> [], value

    member this.Run comp = makeBrick comp

let brick = new BrickBuilder()

(* Transaction

    A transaction can evaluate brick values and set new brick values.

    Inside a transaction, evaluating brick values always resolve to the
    previous state of the brick, never to the values that are changed by 
    the transaction.
*)

// tbd: we could just chain functions here, so TransactionState could be unit -> unit

type Transaction = unit -> unit

type private Write = unit -> unit
type private TransactionState = Write list
type private TransactionM = TransactionState -> TransactionState

type TransactionBuilder() =
    member this.Bind (brick: 'value brick, cont: 'value -> TransactionM) : TransactionM = 
        fun state ->
            let value = brick.evaluate()
            cont value state

    member this.Zero () = id
    member this.Yield _ = id

    member this.Run (t : TransactionM): Transaction = 
        fun () -> 
            let state = t []
            state |> List.rev |> List.iter (fun w -> w())

    member this.For(seq : TransactionM, cont: unit -> TransactionM) : TransactionM =
        fun t ->
            let t = seq t
            cont() t

    [<CustomOperation("write")>]
    member this.Write(nested : TransactionM, brick: 'v brick, value: 'v) =
        fun t ->
            let state = nested t
            (fun () -> brick.write value) :: state
            
    [<CustomOperation("reset")>]
    member this.Reset(nested: TransactionM, brick : 'v Brick) =
        fun t ->
            let state = nested t
            (fun () -> brick.reset()) :: state

(* PROGRAM *)

type Program<'v>(brick : Brick<'v>) =

    let collect() = (brick :> Brick).tryCollect()

    interface IDisposable with
        member i.Dispose() = collect()
            
    member this.run() = brick.evaluate()

    member this.apply(t: Transaction) = 
        t()

type 'v program = Program<'v>

(** BASIC COMBINATORS **)

/// Value brick (tbd: remove in favor of lift?).

let value (v : 'v) : 'v brick =
    brick {
        return v
    }

/// A conversion brick applies a function to a brick.

let convert (c: 's -> 't) (source: 's brick) : 't brick =
    brick {
        let! s = source
        return c s
    }

/// combine two bricks by a funcion.

let combine (c: 'a -> 'b -> 'c) (a: 'a brick) (b: 'b brick) : 'c brick =
    brick {
        let! a = a
        let! b = b
        return c a b
    }

/// lift

type Lifter = Lifter with
   
    static member instance (_:Lifter, v:'v, _:'v brick) : unit -> 'v brick = fun () -> value v
    static member instance (_:Lifter, f: 's -> 't, _:'s brick -> 't brick) = fun () -> convert f
    static member instance (_:Lifter, f: 's -> 'e -> 's, _:'s brick -> 'e brick -> 's brick) =
        fun () ->
            fun (s: 's brick) (e: 'e brick) ->
                brick {
                    let! s = s
                    let! e = e
                    return f s e
                }

let inline lift f = Inline.instance(Lifter,f) ()

// tbd: do we need this anymore?

let liftFolder (f: ('s * 'e) -> 's) =
    fun (s: 's brick, e: 'e brick) ->
        brick {
            let! s = s
            let! e = e
            return f(s,e)
        }

let transaction = new TransactionBuilder()

let valueOf (brick : Brick<'v>) = if brick.valid then brick.value else None
let toProgram b = new Program<_>(b)

[<assembly:AutoOpen("BricksCore")>]
do ()
