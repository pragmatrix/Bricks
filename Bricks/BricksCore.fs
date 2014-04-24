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

and Brick<'v>(computation : Computation<'v>, ?trace : Trace) =
    let mutable _comp = computation
    let mutable _trace : Trace = match trace with Some t -> t | None -> []
    let mutable _value : 'v option = None
    let mutable _alive = false
    let mutable _valid = false
    let mutable _referrer = bset.empty

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

let private makeBrick<'v> f = Brick<'v>(f)
let private makeBrickTrace<'v> t f = Brick<'v>(f, t)

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

(** BRICKS IN TIME **)

(* Memo 

    A memo brick is a brick that is wrapped around another brick and remembers the previous value of it.

    The value of a memo brick is (previousValue, currentValue)
*)


let memo (def:'v) (source:'v brick) : ('v * 'v) brick =

    fun (b:('v*'v) brick) ->
        let value = source.evaluate()
        let prev = 
            match b.value with
            | Some (_, v) -> v
            | None -> def
        let newValue = (prev, value)
        [source :> Brick], newValue
    |> makeBrick

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


(*
    A diff brick is a memo brick and a diff function that is applied to output values of the memo.

    tbd: terminology is confusing: we need another name for diffs over time.
*)

let diff (def: 'v) (differ: 'v -> 'v -> 'd) (source: 'v brick) =
    source |> memo def |> convert (uncurry differ)

module bset =

    let private bDiff = diff

    let diff (s: 'v set brick) =
        bDiff bset.empty bset.diff s

(* 
    Creates a brick that receives the unprocessed sequence of values since the last evaluation.

    This is done by initializing the resulting brick with a trace that points to the chain of
    the source brick and then grabbing the chain for each new evaluation.
*)

(*
let track (f: 'v seq -> 'r) (b: 'v brick) =
    fun (brick : 'r brick) ->
        let sequence = (brick.trace.[0] |> snd) :?> Chain<'v> :> 'v seq
        [b :> Brick, b.chain :> Chain], f sequence |> Chain.single
    |> makeBrickTrace [b :> Brick, b.chain :> Chain]
*)

(* overloaded combinators *)

(*
type b =
    static member map (conv: 'v -> 'w) (d: set.Change<'v> seq brick) =
        ()
*)

(* idset
    An id set is a set that organizes data structures that contain a property named id that returns an object.
    That object represents the identity of the data structure and is used to compare instances of it.
*)

type Id = obj

type IdSet<'v> = HashMap<Id, 'v> 

type 'v idset = IdSet<'v>

//
// Helper that collects brick dependencies.
//

type EvaluationContext() = 
    let mutable _deps : Brick list = List.empty

    member this.evaluate (brick: 'a Brick) = 
        let r = brick.evaluate()
        _deps <- (brick :> Brick) :: _deps
        r

    member this.takeDeps() = 
        let r = _deps
        _deps <- List.empty
        r

module IdSet = 
    
    let private bDiff = diff

    let fromSeq (identify: 'v -> Id) (seq: 'v seq) = HashMap.fromSeq (Seq.map (fun v -> identify v, v) seq)

    type Change<'v> =
        | Added of 'v
        | Modified of 'v
        | Removed of 'v

    type ChangeSet<'v> = Change<'v> seq

    let diff (identify: 'v -> Id) (s1:'v idset) (s2:'v idset) = 
        // tbd: combine finding added / modified
        let added = 
            s2.Values 
            |> Seq.filter (fun v -> identify v |> s1.ContainsKey |> not) 
            |> Seq.map Added

        let modified = 
            s2.Values 
            |> Seq.filter (fun v -> let id = identify v in s1.ContainsKey id && (not (obj.ReferenceEquals(s1.[id],v))) )
            |> Seq.map Modified

        let removed = 
            s1.Values 
            |> Seq.filter (fun v -> identify v |> s2.ContainsKey |> not)
            |> Seq.map Removed

        [removed; modified; added] |> Seq.flatten

    let trackChanges (getId: 'v -> Id) (source : 'v idset brick) = 
        source |> bDiff IdSet.Empty (diff getId)

    (* projector
        A set projector projects set differences to three functions. Its main use is to
        project id based incremental set differences to operating system calls.

        Because a changeset brick may result to the same change set when it did not got rebuilt,
        the projector stores the latest changeset and ignores it if it is provided again.
    *)

    type Projector<'s, 't>
        (
            identify: 's -> Id, 
            added: EvaluationContext -> 's -> 't, 
            modified: EvaluationContext -> 's -> 't -> 't, 
            removed: EvaluationContext -> 's -> 't -> unit
        ) =

        let mutable _latest: ChangeSet<'s> option = None
        let mutable _map: HashMap<Id, 't> = HashMap.Empty

        member this.empty = _map.Count = 0
        member this.values = _map.Values

        member this.project changeSet : Brick list = 
            match _latest with
            | Some latest when isSame latest changeSet -> []
            | _ ->
            let depLists = changeSet |> Seq.map this.projectChange |> Seq.flatten
            _latest <- Some changeSet
            depLists |> Seq.toList

        member private this.projectChange change =
            match change with
            | Added s ->
                let ec = EvaluationContext()
                let t = added ec s
                _map <- _map.Add(identify s, t)
                ec.takeDeps()

            | Modified s -> 
                let id = identify s
                let t = _map.[id]
                let ec = EvaluationContext()
                let t = modified ec s t
                _map <- _map.SetItem(id, t)
                ec.takeDeps()

            | Removed s -> 
                let id = identify s
                let t = _map.[id]
                let ec = EvaluationContext()
                removed ec s t
                _map <- _map.Remove id
                ec.takeDeps()
