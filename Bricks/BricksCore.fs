module BricksCore

(** BRICKS, F# COMPUTATION EXPRESSIONS AND COMBINATORS FOR LAZY INCREMENTAL COMPUTATION, SEE README.MD **)

open BrickDefs
open BrickSet

open System
open System.Diagnostics
open System.Collections.Immutable
open System.Collections.Generic
open System.Linq

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

type Trace = Brick list

type ComputationResult<'v> = Trace * 'v

type Computation<'v> = 'v brick -> ComputationResult<'v>

and 't brick = Brick<'t>

and Brick<'v>(computation : Computation<'v>, ?initial: 'v) =
    let mutable _comp = computation
    let mutable _trace : Trace = []
    let mutable _value : 'v option = initial
    let mutable _alive = false
    let mutable _valid = false
    let mutable _referrer = Set.empty

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

type 't bricks = seq<Brick<'t>>

let makeBrick<'v> f = Brick<'v>(f)
let makeBrickInit<'v> initial f = Brick<'v>(f, initial)

type Manifest<'a> = { instantiator: unit -> 'a }
    with 
        static member create f = { instantiator = f }

let inline manifest f = Manifest<_>.create f

type BrickBuilder() =
    member this.Bind (dependency: 'dep brick, cont: 'dep -> Computation<'next>) : Computation<'next> =
        fun b ->
            let depValue = dependency.evaluate()
            let contDep, contChain = cont depValue b
            dependency:>Brick :: contDep, contChain

    member this.Bind (manifest: Manifest<'i>, cont: 'i -> Computation<'i>) : Computation<'i> =
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


(** BASIC COMBINATORS **)

(*
    Value brick.
*)

let value (v : 'v) : 'v brick =
    brick {
        return v
    }

(*
    A conversion brick applies a function to a brick.
*)

let convert (c: 's -> 't) (source: 's brick) : 't brick =
    brick {
        let! s = source
        return c s
    }

(*
    combine two bricks by a funcion.
*)

let combine (c: 'a -> 'b -> 'c) (a: 'a brick) (b: 'b brick) : 'c brick =
    brick {
        let! a = a
        let! b = b
        return c a b
    }

(* Transaction

    A transaction can evaluate brick values and set new brick values.

    Inside a transaction, evaluating brick values always resolve to the
    previous state of the brick, never to the values that are changed by 
    the transaction.
*)

// tbd: we could just chain functions here, so TransactionState could be unit -> unit

type Write = unit -> unit

type Transaction = unit -> unit
type TransactionState = Write list
type TransactionM = TransactionState -> TransactionState

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

let transaction = new TransactionBuilder()

type ValueOf<'v>(brick:Brick<'v>) = 
    member this.Brick = brick

let valueOf brick = ValueOf(brick)


(* PROGRAM *)

type ProgramM = unit -> Brick list

type Program(_runner : ProgramM) =
    let mutable _deps : Brick list = []

    member this.roots = _deps

    interface IDisposable with
        member this.Dispose() =
            _deps |> List.iter (fun d -> d.tryCollect())
            
    member this.run() =
        let newDeps = _runner()
        _deps.Except(newDeps) |> Seq.iter (fun d -> d.tryCollect())
        _deps <- newDeps

    member this.apply(t: Transaction) = 
        t()

    member this.evaluate (brick: 'v brick) : 'v = 
        brick.evaluate()

type ProgramBuilder() =

    (* A regular let! is the isolated evaluation of a root brick in the context of the program *)

    member this.Bind (brick: 'value brick, cont: 'value -> ProgramM) : ProgramM = 
        fun () ->
            let value = brick.evaluate()
            brick :> Brick :: cont value ()

    member this.Bind (vo: ValueOf<'value>, cont: 'value option -> ProgramM) : ProgramM =
        fun () ->
            let brick = vo.Brick
            let v = if brick.valid then vo.Brick.value else None
            cont v ()

    // for do! operations that only add some new dependencies

    member this.Bind (deps: 'a brick list, cont: unit -> ProgramM) : ProgramM =
        this.Bind(deps |> List.map (fun b -> b :> Brick), cont)

    member this.Bind (deps: Brick list, cont: unit -> ProgramM) : ProgramM =
        fun () ->
            deps @ cont () ()

    member this.Zero () = fun () -> []
    member this.Yield _ = fun () -> []
    member this.Return _ = fun () -> []

    member this.Run p = new Program(p)

    member this.For(seq : ProgramM, cont: unit -> ProgramM) : ProgramM =
        fun () ->
            let deps = seq ()
            deps @ cont() ()

    [<CustomOperation("apply")>]
    member this.Apply(nested : ProgramM, transaction: Transaction) = 
        fun () ->
            let deps = nested ()
            transaction()
            deps

let program = new ProgramBuilder()
