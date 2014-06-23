module BricksCore

(** BRICKS, F# COMPUTATION EXPRESSIONS AND COMBINATORS FOR LAZY INCREMENTAL COMPUTATION, SEE README.MD **)

open System
open System.Diagnostics
open System.Collections.Immutable
open System.Collections.Generic
open System.Linq

open BrickCollections
open InlineHelper

(** BRICK, ENVIRONMENT **)

type Versioned = interface
    end

and Brick =
    abstract member addReferrer : Brick -> unit
    abstract member removeReferrer : Brick -> unit
    abstract member tryCollect : unit -> unit
    abstract member invalidate : unit -> unit
    abstract member versioned : Versioned with get

[<AutoOpen>]
module BrickExtensions =
    type Brick with
        member this.removeReferrerAndTryCollect referrer =
            this.removeReferrer referrer
            this.tryCollect()

// we could use Chain for that.

type Versioned<'v> = { value: 'v; mutable next: Versioned<'v> option } with

    interface Versioned

    static member single (value: 'v) = { value = value; next = None }

    static member ofSeq (s: 'v seq) =
        let h = Versioned.single (Seq.head s)
        h.pushSeq (Seq.skip 1 s) |> ignore
        h

    member this.push (value: 'v) =
        assert(this.next.IsNone)
        let n = Versioned<'v>.single value
        this.next <- Some n
        n

    member this.head = this.value

    member this.tail =
        match this.next with
        | None -> []
        | Some next -> next.value :: next.tail

    member this.pushSeq values = 
        values |> Seq.fold (fun (c : Versioned<_>) v -> c.push v) this


type internal Trace = Brick list

type History<'v> = 
    | Reset of 'v
    | Progress of 'v seq

type internal ComputationResult<'v> = Trace * 'v list

type internal Computation<'v> = 'v brick -> ComputationResult<'v>

and 't brick = Brick<'t>

and Brick<'v>(computation : Computation<'v>) =
    
    let mutable _comp = computation
    let mutable _trace : IMap<Brick, Versioned> = IMap.empty
    let mutable _versioned : Versioned<'v> option = None
    let mutable _alive = false
    let mutable _valid = false
    let mutable _referrer = ISet.empty

    member internal this.valid = _valid
    member internal this.value = _versioned |> Option.map(fun v -> v.value) 
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
                match box this.value.Value with
                | :? IDisposable as d -> d.Dispose()
                | _ -> ()
                _trace.Keys |> Seq.iter (fun dep -> dep.removeReferrerAndTryCollect this)

        member this.versioned = _versioned.Value :> Versioned

    member this.evaluate() : 'v = 
        if _valid then this.value.Value else
        let t, c = _comp this
        // relink referrers (tbd: do this incrementally)
        _trace.Keys |> Seq.iter (fun dep -> dep.removeReferrer this)
        t |> Seq.iter (fun dep -> dep.addReferrer this)

        _trace.Keys |> Seq.iter (fun dep -> dep.tryCollect())

        _trace <- t |> Seq.map (fun dep -> dep, dep.versioned) |> IMap.ofSeq

        _versioned <-        
            match _versioned with
            | None -> Versioned<'v>.single (Seq.last c)
            | Some v -> v.pushSeq c
            |> Some
        
        _alive <- true
        _valid <- true
        _versioned.Value.value

    member this.history (dep : 'd brick) : History<'d> = 
        let depV = dep.evaluate()
        match _trace.get dep with
        | None -> Reset depV
        | Some (:? Versioned<'d> as v) -> Progress v.tail
        | _ -> failwith "internal error"

    member this.previous (dep : 'd brick) : 'd option = 
        match _trace.get dep with
        | None -> None
        | Some (:? Versioned<'d> as v) -> Some v.head
        | _ -> failwith "internal error"

    member this.write v =
        this.invalidate()
        _comp <- fun _ -> [], [v]

    member this.reset() = 
        this.invalidate()
        _comp <- computation

    member this.invalidate() =
        (this :> Brick).invalidate()    

type 't bricks = seq<'t brick>

let internal makeBrick<'v> f = Brick<'v>(f)

// i want these in a module named Brick

type ManifestMarker<'a> = ManifestMarker of (unit -> 'a)
type SelfValueMarker = SelfValueMarker
type HistoryMarker<'v> = HistoryMarker of Brick<'v>
type PreviousMarker<'v> = PreviousMarker of Brick<'v>

let inline manifest f = ManifestMarker f
let valueOfSelf = SelfValueMarker
let inline historyOf b = HistoryMarker b
let inline previousOf b = PreviousMarker b


type BrickBuilder() =
    member this.Bind (dependency: 'dep brick, cont: 'dep -> Computation<'next>) : Computation<'next> =
        fun b ->
            let depValue = dependency.evaluate()
            let contDep, r = cont depValue b
            dependency:>Brick :: contDep, r

    member this.Bind (dependencies: 'dep brick seq, cont: 'dep seq -> Computation<'next>) : Computation<'next> =
        fun b ->
            let depValues = dependencies |> Seq.map (fun d -> d.evaluate())
            let contDep, r = cont depValues b
            let thisDeps = dependencies |> Seq.cast |> Seq.toList
            thisDeps @ contDep, r

    member this.Bind (ManifestMarker instantiator, cont: 'i -> Computation<'i>) : Computation<'i> =
        fun b ->
            let i = 
                match b.instance with
                | Some i -> i
                | None -> instantiator()
            cont i b

    member this.Bind (SelfValueMarker, cont: 'v option -> Computation<'v>) : Computation<'v> =
        fun b ->
            let v = b.value
            cont v b

    member this.Bind (HistoryMarker dep, cont: History<'d> -> Computation<'v>) : Computation<'v> =
        fun b ->
            let h = b.history dep
            let contDep, r = cont h b
            dep:>Brick :: contDep, r

    member this.Bind (PreviousMarker dep, cont: 'd option -> Computation<'v>) : Computation<'v> =
        fun b ->
            // note: we don't add the brick to the list of dependencies.
            let p = b.previous dep
            let contDep, r = cont p b
            contDep, r

    member this.Return value = fun _ -> [], [value]
    member this.Yield value = this.Return value

    member this.ReturnFrom (brick: 'value brick) = 
        fun _ ->
            let value = brick.evaluate();
            [brick:>Brick], [value]

    member this.YieldFrom (dep: 'dep brick) = this.ReturnFrom dep

    // seeing History as a separate category, we can justify using yield! instead of yield. 
    // yield can not be overloaded.
    member this.ReturnFrom (h: History<'v>) =
        match h with 
        | Reset v -> this.Return v
        | Progress p -> fun _ -> [], p |> Seq.toList

    member this.YieldFrom (h: History<'v>) =
        match h with 
        | Reset v -> this.Yield v
        | Progress p -> fun _ -> [], p |> Seq.toList

    member this.Combine (first: Computation<'v>, second: Computation<'v>) =
        fun b ->
            let (fd, fv) = first b
            let (sd, sv) = second b
            fd @ sd, fv @ sv


    [<CustomOperation("yieldSeq")>]
    member this.Write(nested : Computation<'v>, sequence: 'v seq) =
        fun b ->
            let (nd, nv) = nested b
            nd, nv @ (sequence |> Seq.toList)

    member this.Run comp = makeBrick comp

    member this.Delay (f: unit -> Computation<'v>) : Computation<'v> = 
        fun b ->
            f () b

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
/// could be a map override?

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
