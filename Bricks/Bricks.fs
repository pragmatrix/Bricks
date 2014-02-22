﻿module Bricks

open System
open System.Diagnostics
open System.Collections.Immutable
open System.Collections.Generic
open System.Linq

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

module List =
    let flatten l = List.collect id l

(* Brick, Referrer, Environment *)

type Brick = interface end


let defaultInvalidator env _ = env

type private BrickInvalidator = Environment -> Brick -> Environment

and BrickState = 
    { value: obj; trace: Brick list; invalidator: BrickInvalidator; referrer: Brick set }
    with 
        member this.addReferrer referrer =
            { this with referrer = this.referrer.Add referrer }
        member this.removeReferrer referrer = 
            { this with referrer = this.referrer.Remove referrer }

        static member make value trace invalidator =
            { value= value; trace = trace; invalidator = invalidator; referrer = set.empty }

and Environment = 
    {
        values: HashMap<Brick, BrickState>;
        orphans: Brick set;
    }
    with 
        member this.add brick state =
            let trace = state.trace
            let this = trace |> List.fold (fun (env:Environment) r -> env.addReferrer r brick) this
            let values = this.values.Add(brick, state)
            { this with values = values }
        
        member this.invalidate (bricks : Brick list) =

            match bricks with
            | [] -> this
            | brick::rest ->

            match this.values.get brick with
            | None -> this.invalidate rest
            | Some {value=value; trace=trace; invalidator=invalidator; referrer=referrer} ->
                
            let this = {this with values = this.values.Remove brick }

            (* remove all referrer *)
            let this = trace |> List.fold (fun (env:Environment) r -> env.removeReferrer r brick ) this

            let orphans = trace |> List.filter (fun dep -> this.hasValue dep && not (this.hasReferrer dep))
            let this = { this with orphans = this.orphans.Union orphans }

            (* call the invalidator as late as possible so that it sees a consistently connected graph *)
            let this = invalidator this brick

            this.invalidate (rest @ (Seq.toList referrer))

        member this.commitWrites writes = 
            let commitValue (this:Environment) (brick, value_)  =
                let this = this.invalidate [brick]
                match value_ with
                | None -> this
                | Some value -> this.add brick (BrickState.make value [] defaultInvalidator)

            writes |> List.fold commitValue this

        member this.collect() =
            let rec collectRec this = 
                if this.orphans.Count = 0 then this else
                let batch = this.orphans |> Seq.filter (this.hasReferrer >> not) |> Seq.toList
                let this = { this with orphans = set.empty }
                let this = this.invalidate batch
                collectRec this

            collectRec this

        member this.hasValue b = this.values.has b

        member private this.addReferrer brick referrer =
            match this.values.get brick with
            | None -> this
            | Some state ->
            let state = state.addReferrer referrer
            this.setState brick state
            
        member private this.removeReferrer brick referrer = 
            match this.values.get brick with
            | None -> this
            | Some state ->
            let state = state.removeReferrer referrer
            this.setState brick state

        member private this.hasReferrer brick = 
            match this.values.get brick with
            | None -> false
            | Some state -> state.referrer.Count <> 0

        member private this.setState brick state =
            { this with values = this.values.SetItem(brick, state) }


        static member empty = 
            { values = HashMap.Empty; orphans = set.Empty }

type Computation = Environment -> (Environment * BrickState)

type Brick<'v>(f : Computation) =
    interface Brick
    with 
        member this.evaluate (env:Environment) : Environment * 'v = 
            let values = env.values
            if values.ContainsKey this then
                env, unbox values.[this].value
            else
            let env, state = f env
            let v = unbox state.value
            let env = env.add this state
            env, v
              

type 't brick = Brick<'t>

let private makeBrick<'v> f = Brick<'v>(f)

type BrickBuilder() =
    (* tbd: instead of chaining the computation expression internal closure bricks, we could combine them *)
    member this.Bind (dependency: 'dep brick, cont: 'dep -> 'next brick) : 'next brick =
        fun env ->
            let env, depValue = dependency.evaluate env;
            let contBrick = cont depValue
            let env, value = contBrick.evaluate env
            env , BrickState.make value [dependency] defaultInvalidator
        |> makeBrick

    (* need to wrap this in a new brick here, to allow a shadow write later on *)
    member this.ReturnFrom (brick: 'value brick) = 
        fun env ->
            let env, value = brick.evaluate env;
            env, BrickState.make value [brick] defaultInvalidator
        |> makeBrick

    member this.Return value = 
        fun env -> env, BrickState.make value [] defaultInvalidator
        |> makeBrick

let brick = new BrickBuilder()

(* Program *)

type Write = Brick * (obj option)

type Program = 
    { 
        env: Environment; 
    }
    with
        member this.evaluate (brick: 'v brick) : Program * 'v = 
            let env,v = brick.evaluate this.env
            { this with env = env }, v

        member this.collect() : Program =
            { this with env = this.env.collect() }

        static member empty = { env = Environment.empty }


(* Transaction

    A transaction can evaluate brick values and set new brick values.

    Inside a transaction, evaluating brick values always resolve to the
    previous state of the brick, never to the values that are changed by 
    the transaction.
*)

type Transaction = Environment * Write list
type TransactionM = Transaction -> Transaction

type TransactionBuilder() =
    member this.Bind (brick: 'value brick, cont: 'value -> TransactionM) : TransactionM = 
        fun (env, wl) ->
            let env, value = brick.evaluate env
            cont value (env, wl)

    member this.Zero () = id
    member this.Yield _ = id

    member this.Run (t : TransactionM): (Environment -> Environment) = 
        fun env -> 
            let (env, wl) = t (env, [])
            wl |> List.rev |> env.commitWrites

//    member this.Return p = p

    member this.For(seq : TransactionM, cont: unit -> TransactionM) : TransactionM =
        fun t ->
            let t = seq t
            cont() t

    [<CustomOperation("write")>]
    member this.Write(nested : TransactionM, brick: 'v brick, value: 'v) =
        fun t ->
            let (env, wl) = nested t
            (env, (brick :> Brick, Some (value :> obj)) :: wl)
            
    [<CustomOperation("reset")>]
    member this.Reset(nested: TransactionM, brick : Brick) =
        fun t ->
            let (env, wl) = nested t
            (env, (brick, None) :: wl)

let transaction = new TransactionBuilder()

type ProgramM = Program -> Program

type ValueOf<'v>(brick:Brick<'v>) = 
    member this.Brick = brick

let valueOf brick = ValueOf(brick)

type ProgramBuilder() =

    (* A regular let! is the isolated evaluation of a root brick in the context of the program *)

    member this.Bind (brick: 'value brick, cont: 'value -> ProgramM) : ProgramM = 
        fun p ->
            let p, v = p.evaluate brick
            cont v p

    member this.Bind (vo: ValueOf<'value>, cont: 'value option -> ProgramM) : ProgramM =
        fun p ->
            let env = p.env
            let hasValue, evaluationResult = env.values.TryGetValue vo.Brick
            let v = if hasValue then Some (unbox evaluationResult.value) else None
            cont v p

    member this.Zero () = id
    member this.Yield _ = id

    member this.For(seq : ProgramM, cont: unit -> ProgramM) : ProgramM =
        fun p ->
            let p = seq p
            cont() p

    [<CustomOperation("apply")>]
    member this.Apply(nested : ProgramM, transaction: (Environment -> Environment)) = 
        fun p ->
            let p = nested p
            {p with env = transaction p.env}

    [<CustomOperation("collect")>]
    member this.Collect(nested : ProgramM) =
        fun p ->
            let p = nested p
            p.collect()

let program = new ProgramBuilder()

(* Memo 

    A memo brick is a brick that is wrapped around another brick and remembers the previous value of it.

    The value of a memo brick is (previousValue, currentValue)
*)

let memo (target:'v brick) (def:'v) : ('v * 'v) brick =

    let memoBrick = brick { return def }
    fun env ->
            
        let env, pv = memoBrick.evaluate env
        let env, v = target.evaluate env
        let invalidator env brick = 
            let env = defaultInvalidator env brick
            let t = transaction { write memoBrick v }
            t env

        env, BrickState.make (pv, v) [memoBrick; target] invalidator
    |> makeBrick


(* idset
    An id set is a set that organizes data structures that contain a property named id that returns an object.
    That object represents the identity of the data structure and is used to compare instances of it.
*)

type IdSet<'v when 'v:(member id:obj)> = HashMap<obj, 'v> 

type 'v idset when 'v:(member id:obj) = IdSet<'v>

module IdSet = 
    type Diff<'v> = { added: 'v seq; removed: 'v seq; modified: 'v seq }

    let inline id v = (^v: (member id:obj) v)

    let inline diff (s1:'v idset) (s2:'v idset) = 
        // tbd: combine finding added / modified
        let added = s2.Values |> Seq.filter (fun v -> id v |> s1.ContainsKey |> not)
        let removed = s1.Values |> Seq.filter (fun v -> id v |> s2.ContainsKey |> not)
        let modified = s2.Values |> Seq.filter (fun v -> let id = id v in s1.ContainsKey id && (not (obj.ReferenceEquals(s1.[id],v))) )
        { added = added; removed = removed; modified = modified}
