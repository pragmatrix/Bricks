﻿module Bricks

open System
open System.Diagnostics
open System.Collections.Immutable
open System.Collections.Generic

(* *)

type HashSet = ImmutableHashSet
type HashSet<'k> = ImmutableHashSet<'k>
type 'k set = HashSet<'k>
module set = 
    let empty<'e> = HashSet<'e>.Empty
    let ofSeq = HashSet.CreateRange

type HashMap = ImmutableDictionary
type HashMap<'k, 'v> = ImmutableDictionary<'k, 'v>

module List =
    let flatten l = List.collect id l

(* Brick, Referrer, Environment *)

type Brick = interface end


type private ReferrerMap = HashMap<Brick, Brick set>

let private addReferrer (map : ReferrerMap) (brick:Brick) referrer =
    let hasSet, set = map.TryGetValue referrer
    if hasSet then
        map.SetItem(referrer, set.Add brick)
    else
        map.Add(referrer, HashSet.Create brick)

let private removeReferrer (map : ReferrerMap) brick referrer = 
    let hasReferrer, set = map.TryGetValue referrer
    if not hasReferrer then map else
    map.SetItem(referrer, set.Remove brick)

let defaultInvalidator env _ = env

type private BrickInvalidator = Environment -> Brick -> Environment

and EvaluationResult<'v> = 
    { value: 'v; trace: Brick list; invalidator: BrickInvalidator }

and Environment = 
    {
        values: HashMap<Brick, EvaluationResult<obj> >;
        referrer: ReferrerMap    
    }
    with 
        member this.add brick result =
            let {trace=trace;value=value} = result
            let referrer = trace |> List.fold (fun refs -> addReferrer refs brick) this.referrer
            let values = this.values.Add(brick, result)
            { this with values = values; referrer = referrer}
        
        member this.invalidate (bricks : Brick list) =
            let rec invalidateRec this (bricks : Brick list) = 
                match bricks with
                | [] -> this
                | brick::rest ->
                let hasBrick, r = this.values.TryGetValue brick
                if not hasBrick then invalidateRec this rest else
                let {value=value; trace=trace;invalidator=invalidator} = r;
                
                let hasReferrer, bricksReferrer = this.referrer.TryGetValue brick
                let bricksReferrer = if hasReferrer then bricksReferrer |> Seq.toList else []

                (* remove the value first to avoid endless recursions, for example if the invalidator causes the very same value to be invalidated *)
                let this = {this with values = this.values.Remove brick }
                
                (* call the invalidator *)
                let this = invalidator this brick

                (* remove all referrer *)
                let referrer = trace |> List.fold (fun referrer dep -> removeReferrer referrer brick dep) this.referrer
                let referrer = brick |> referrer.Remove

                
                let this = { this with referrer = referrer }
                invalidateRec this (rest @ bricksReferrer)

            bricks |> invalidateRec this
           
        member this.commitWrites writes = 
            let commitValue (this:Environment) (brick, value_)  =
                let this = this.invalidate [brick]
                match value_ with
                | None -> this
                | Some value -> this.add brick { EvaluationResult.value = value; trace = []; invalidator = defaultInvalidator }

            writes |> List.fold commitValue this
 

        static member empty = 
            { values = ImmutableDictionary.Empty; referrer = ImmutableDictionary.Empty }

type ComputationResult<'v> = Environment * EvaluationResult<'v>

type Computation<'v> = Environment -> ComputationResult<'v>

type Brick<'v>(f : Computation<'v>) =
    interface Brick
    with 
        member this.evaluate (env:Environment) : Environment * 'v = 
            let values = env.values
            if values.ContainsKey this then
                env, values.[this].value :?> 'v
            else
            let env, res = f env
            let v = res.value
            let env = env.add this {value = v:>obj; trace = res.trace; invalidator = res.invalidator }
            env, v
              

type 't brick = Brick<'t>

let private makeBrick f = Brick<_>(f)

type BrickBuilder() =
    (* tbd: instead of chaining the computation expression internal closure bricks, we could combine them *)
    member this.Bind (dependency: 'dep brick, cont: 'dep -> 'next brick) : 'next brick =
        fun env ->
            let env, depValue = dependency.evaluate env;
            let contBrick = cont depValue
            let env, value = contBrick.evaluate env
            env , { trace = [dependency]; value = value; invalidator = defaultInvalidator }
        |> makeBrick

    (* need to wrap this in a new brick here, to allow a shadow write later on *)
    member this.ReturnFrom (brick: 'value brick) = 
        fun env ->
            let env, value = brick.evaluate env;
            env, { trace = [brick]; value = value; invalidator = defaultInvalidator }
        |> makeBrick

    member this.Return value = 
        fun env -> env, { trace = []; value = value; invalidator = defaultInvalidator }
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

    [<CustomOperation("write", MaintainsVariableSpace = true)>]
    member this.Write(nested : TransactionM, brick: 'v brick, value: 'v) =
        fun t ->
            let (env, wl) = nested t
            (env, (brick :> Brick, Some (value :> obj)) :: wl)
            
    [<CustomOperation("reset", MaintainsVariableSpace = true)>]
    member this.Reset(nested: TransactionM, brick : Brick) =
        fun t ->
            let (env, wl) = nested t
            (env, (brick, None) :: wl)

let transaction = new TransactionBuilder()

type ProgramM = Program -> Program

type ProgramBuilder() =

    (* A regular let! is the isolated evaluation of a root brick in the context of the program *)

    member this.Bind (brick: 'value brick, cont: 'value -> ProgramM) : ProgramM = 
        fun p ->
            let p, v = p.evaluate brick
            cont v p

    member this.Zero () = id
    member this.Yield _ = id

    member this.For(seq : ProgramM, cont: unit -> ProgramM) : ProgramM =
        fun p ->
            let p = seq p
            cont() p

    [<CustomOperation("apply", MaintainsVariableSpace = true)>]
    member this.Apply(nested : ProgramM, transaction: (Environment -> Environment)) = 
        fun p ->
            let p = nested p
            {p with env = transaction p.env}

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

        env, { trace = [memoBrick; target]; value = (pv, v); invalidator = invalidator }
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
