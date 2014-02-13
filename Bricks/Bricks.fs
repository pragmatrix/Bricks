module Bricks

open System
open System.Diagnostics
open System.Collections.Immutable

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

type BrickInvalidator = Environment -> Brick -> Environment

and EvaluationResult = 
    { value: obj; trace: Brick list; invalidator: BrickInvalidator }

and Environment = 
    {
        values: HashMap<Brick, EvaluationResult>;
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
                let hasBrick, {value=value; trace=trace;invalidator=invalidator} = this.values.TryGetValue brick
                if not hasBrick then invalidateRec this rest else
                
                let hasReferrer, referrer = this.referrer.TryGetValue brick
                let bricksReferrer = if hasReferrer then referrer |> Seq.toList else []
                
                (* before doing anything, call the invalidator *)
                let this = invalidator this brick

                (* remove all referrer *)
                let referrer = trace |> List.fold (fun referrer dep -> removeReferrer referrer brick dep) this.referrer
                let referrer = brick |> referrer.Remove

                (* remove the value *)
                let values = this.values.Remove brick
                
                let this = { this with values = values; referrer = referrer }
                invalidateRec this (rest @ bricksReferrer)

            bricks |> invalidateRec this
           
        static member empty = 
            { values = ImmutableDictionary.Empty; referrer = ImmutableDictionary.Empty }

type ComputationResult<'v> = { env: Environment; trace: Brick list; value: 'v }

type Computation<'v> = Environment -> ComputationResult<'v>

type Brick<'v>(f : Computation<'v>) =
    interface Brick
    with 
        member this.evaluate (env:Environment) : Environment * 'v = 
            let values = env.values
            if values.ContainsKey this then
                env, values.[this].value :?> 'v
            else
            let res = f env
            let v = res.value
            let newEnv = res.env.add this {value = v:>obj; trace = res.trace; invalidator = defaultInvalidator }
            newEnv, v
              

type 't brick = Brick<'t>

let private makeBrick f = Brick<_>(f)

type BrickBuilder() =
    (* tbd: instead of chaining the computation expression internal closure bricks, we could combine them *)
    member this.Bind (dependency: 'dep brick, cont: 'dep -> 'next brick) : 'next brick =
        fun env ->
            let env, depValue = dependency.evaluate env;
            let contBrick = cont depValue
            let env, value = contBrick.evaluate env
            { env = env; trace = [dependency]; value = value }
        |> makeBrick

    (* need to wrap this in a new brick here, to allow a shadow write later on *)
    member this.ReturnFrom (brick: 'value brick) = 
        fun env ->
            let env, value = brick.evaluate env;
            { env = env; trace = [brick]; value = value }
        |> makeBrick

    member this.Return value = 
        fun env -> { env = env; trace = []; value = value }
        |> makeBrick

let brick = new BrickBuilder()

(* Program *)

type Write = Brick * (obj option)

type Program = 
    { 
        env: Environment; 
        pendingWrites: Write list;
    }
    with
        member this.evaluate (brick: 'v brick) : Program * 'v = 
            let env,v = brick.evaluate this.env
            { this with env = env }, v

        member this.write (brick: 'v brick, value: 'v) =
            { this with pendingWrites = (brick :> Brick, Some (value :> obj)) :: this.pendingWrites }

        member this.reset brick = 
            { this with pendingWrites = (brick, None) :: this.pendingWrites }

        member this.commitWrites = 
            let commitValue (this:Program) (brick, value_)  =
                let env = this.env.invalidate [brick]
                match value_ with
                | None -> {this with env = env}
                | Some value -> {this with env = env.add brick {value = value; trace = []; invalidator = defaultInvalidator } }

            let this = this.pendingWrites |> List.fold commitValue this
            { this with pendingWrites = [] }

 
        static member empty = { env = Environment.empty; pendingWrites = []}


(* Transaction

    A transaction can evaluate brick values and set new brick values.

    Inside a transaction, evaluating brick values always resolve to the
    previous state of the brick, never to the values that are changed by 
    the transaction.
*)

type Transaction = Program -> Program

type TransactionBuilder() =
    member this.Bind (brick: 'value brick, cont: 'value -> Transaction) : Transaction = 
        fun p ->
            let p, value = p.evaluate brick
            cont value p

    member this.Zero () = id
    member this.Yield _ = id

    member this.Run (t : Transaction): Transaction = 
        fun p -> 
            let p = t p
            p.commitWrites

//    member this.Return p = p

    member this.For(seq : Transaction, cont: unit -> Transaction) : Transaction =
        fun p ->
            let p = seq p
            cont() p

    [<CustomOperation("write", MaintainsVariableSpace = true)>]
    member this.Write(nested : Transaction, brick: 'v brick, value: 'v) =
        fun (p: Program) ->
            let p = nested p
            p.write (brick, value)
            
    [<CustomOperation("reset", MaintainsVariableSpace = true)>]
    member this.Reset(nested: Transaction, brick : Brick) =
        fun (p: Program) ->
            let p = nested p
            p.reset brick

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
    member this.Apply(nested : ProgramM, transaction: Transaction) = 
        fun p ->
            let p = nested p
            transaction p

let program = new ProgramBuilder()

(* Diffs *)




