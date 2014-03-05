module Bricks

(** BRICKS, F# COMPUTATION EXPRESSIONS AND COMBINATORS FOR LAZY INCREMENTAL COMPUTATION, SEE README.MD **)

open System
open System.Diagnostics
open System.Collections.Immutable
open System.Collections.Generic
open System.Linq

open Chain

(* *)

type HashSet = ImmutableHashSet
type HashSet<'k> = ImmutableHashSet<'k>
type 'k set = HashSet<'k>

module set = 
    let empty<'e> = HashSet<'e>.Empty
    let ofSeq = HashSet.CreateRange

type HashMap = ImmutableDictionary
type HashMap<'k, 'v> = ImmutableDictionary<'k, 'v>

type ImmutableHashSet<'v> with
    member this.has v = this.Contains v

type ImmutableDictionary<'k, 'v> with
    member this.get k = 
        let has, v = this.TryGetValue k
        if has then Some v else None
    member this.has k = 
        this.ContainsKey k

type ImmutableDictionary with
    static member fromSeq seq = 
        ImmutableDictionary.CreateRange(Seq.map (fun (k, v) -> KeyValuePair(k, v)) seq)

module List =
    let flatten l = List.collect id l

module Seq = 
    let flatten l = Seq.collect id l

let inline curry f a b = f (a, b)
let inline uncurry f (a, b) = f a b
let inline private (|?) (a: 'a option) b = if a.IsSome then a.Value else b

let isSame a b = obj.ReferenceEquals(a, b)

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

type Trace = (Brick * Chain) list

type ComputationResult<'v> = Trace * 'v chain

type Computation<'v> = Brick<'v> -> ComputationResult<'v>

and Brick<'v>(f : Computation<'v>) =
    let mutable _trace : Trace = []
    let mutable _chain : 'v chain = Chain.empty()
    let mutable _valid = false
    let mutable _referrer = set.empty

    member this.chain = _chain
    member internal this.valid = _valid
    member internal this.value : 'v option =
        if (_chain.atEnd) then None else Some _chain.value

    interface Brick with
        member this.addReferrer r =
            _referrer <- _referrer.Add r

        member this.removeReferrer r =
            _referrer <- _referrer.Remove r

        member this.invalidate() =
            if (not _valid) then () else
            _valid <- false
            _referrer |> Seq.iter (fun b -> b.invalidate())

        member this.tryCollect() =
            if (not _valid || _referrer.Count <> 0) then () else
            (this:>Brick).invalidate()
            _trace |> List.iter (fun (dep, _) -> dep.removeReferrerAndTryCollect this)

    member this.evaluate() : 'v chain = 
        if _valid then _chain else
        let t, c = f this
        // relink referrers (tbd: do this incrementally)
        _trace |> List.iter (fun (dep, _) -> dep.removeReferrer this)
        t |> List.iter (fun (dep, _) -> dep.addReferrer this)

        _trace |> List.iter (fun (dep, _) -> dep.tryCollect())

        _trace <- t
        _chain <- c
        _valid <- true
        c

    member this.write v =
        this.invalidate()
        _trace |> List.iter (fun (dep, _) -> dep.removeReferrerAndTryCollect this)
        _trace <- []
        _chain <- _chain.append v
        _valid <- true

    member this.reset() = 
        this.invalidate()

    member this.invalidate() =
        (this :> Brick).invalidate()

    

type 't brick = Brick<'t>

let private makeBrick<'v> f = Brick<'v>(f)

let private chainValue c = c.next.Value |> fst

type BrickBuilder() =
    (* tbd: instead of chaining the computation expression internal closure bricks, we could combine them *)
    member this.Bind (dependency: 'dep brick, cont: 'dep -> 'next brick) : 'next brick =
        fun _ ->
            let depChain = dependency.evaluate()
            let contBrick = cont (depChain.value)
            let chain = contBrick.evaluate()
            [(dependency:>Brick, depChain:>Chain); (contBrick:>Brick, chain:>Chain)], chain
        |> makeBrick

    (* need to wrap this in a new brick here, to allow a shadow write later on *)
    member this.ReturnFrom (brick: 'value brick) = 
        fun _ ->
            let value = brick.evaluate();
            [brick:>Brick, value:>Chain], value
        |> makeBrick

    member this.Return value = 
        fun _ -> [], Chain.single value
        |> makeBrick

let brick = new BrickBuilder()

(* PROGRAM *)

type Program() =
    member this.evaluate (brick: 'v brick) : 'v = 
        brick.evaluate() |> chainValue

    member this.collect() : Program =
        this

    static member empty = Program()


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
            let chain = brick.evaluate()
            cont chain.value state

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

type ProgramM = Program -> Program

type ValueOf<'v>(brick:Brick<'v>) = 
    member this.Brick = brick

let valueOf brick = ValueOf(brick)

type ProgramBuilder() =

    (* A regular let! is the isolated evaluation of a root brick in the context of the program *)

    member this.Bind (brick: 'value brick, cont: 'value -> ProgramM) : ProgramM = 
        fun p ->
            let chain = brick.evaluate()
            cont (chain.value) p

    member this.Bind (vo: ValueOf<'value>, cont: 'value option -> ProgramM) : ProgramM =
        fun p ->
            let brick = vo.Brick
            let v = if brick.valid then vo.Brick.value else None
            cont v p

    member this.Zero () = id
    member this.Yield _ = id

    member this.For(seq : ProgramM, cont: unit -> ProgramM) : ProgramM =
        fun p ->
            let p = seq p
            cont() p

    [<CustomOperation("apply")>]
    member this.Apply(nested : ProgramM, transaction: Transaction) = 
        fun p ->
            let p = nested p
            transaction()
            p

let program = new ProgramBuilder()

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
    With combine, two bricks can be combined by a funcion.
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
        let chain = source.evaluate()
        let prev = 
            match b.value with
            | Some (_, v) -> v
            | None -> def
        let newValue = (prev, chain.value)
        [source :> Brick, chain :> Chain], b.chain.append newValue
    |> makeBrick

(*
    A diff brick is a memo brick and a diff function that is applied to output values of the memo.
*)

let diff (def: 'v) (diff: 'v -> 'v -> 'd) (source: 'v brick) =
    source |> memo def |> convert (uncurry diff)

(* idset
    An id set is a set that organizes data structures that contain a property named id that returns an object.
    That object represents the identity of the data structure and is used to compare instances of it.
*)

type Id = obj

type IdSet<'v> = HashMap<Id, 'v> 

type 'v idset = IdSet<'v>


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
            added: 's -> 't, 
            modified: 's -> 't -> 't, 
            removed: 's -> 't -> unit
        ) =

        let mutable latest: ChangeSet<'s> option = None
        let mutable map: HashMap<Id, 't> = HashMap.Empty

        member this.empty = map.Count = 0
        member this.values = map.Values

        member this.project changeSet = 
            match latest with
            | Some latest when isSame latest changeSet -> ()
            | _ ->
            changeSet |> Seq.iter this.projectChange
            latest <- Some changeSet

        member private this.projectChange change =
            match change with
            | Added s ->
                let t = added s
                map <- map.Add(identify s, t)
            | Modified s -> 
                let id = identify s
                let t = map.[id]
                let t = modified s t
                map <- map.SetItem(id, t)
            | Removed s -> 
                let id = identify s
                let t = map.[id]
                removed s t
                map <- map.Remove id
