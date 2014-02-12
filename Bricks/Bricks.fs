module Bricks

open System
open System.Diagnostics
open System.Collections.Immutable

(* *)

type HashSet = ImmutableHashSet
type HashSet<'k> = ImmutableHashSet<'k>
type 'k set = HashSet<'k>

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

type Environment = 
    {
        values: HashMap<Brick, obj * Brick list>;
        referrer: ReferrerMap    
    }
    with 
        member this.add brick value =
            let _,trace = value
            let referrer = trace |> List.fold (fun refs -> addReferrer refs brick) this.referrer
            let values = this.values.Add(brick, value)
            { this with values = values; referrer = referrer}
        
        member this.remove brick = 
            let _,trace = this.values.[brick]
            let referrer = trace |> List.fold (fun referrer dep -> removeReferrer referrer brick dep) this.referrer
            let referrer = brick |> referrer.Remove
            let values = this.values.Remove brick
            { this with values = values; referrer = referrer }
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
                env, values.[this] |> fst :?>'v
            else
            let res = f env
            let v = res.value
            let newEnv = res.env.add this (v:>obj, res.trace)
            newEnv, v

type 't brick = Brick<'t>

type BrickBuilder() =
    member this.Bind (dependency: 'dep brick, cont: 'dep -> 'next brick) : 'next brick =
        let f = fun env ->
            let env, depValue = dependency.evaluate env;
            let contBrick = cont depValue
            let env, value = contBrick.evaluate env
            { env = env; trace = [dependency]; value = value }
        Brick<'next>(f)
    member this.Return value = Brick(fun env -> { env = env; trace = []; value = value })

let brick = new BrickBuilder()

(* Program *)

type Write = Brick * (obj option)

type Program = 
    { 
        env: Environment; 

        // pending writes
        newWrites: Write list;
    }
    with
        member this.evaluate (brick: 'v brick) : Program * 'v = 
            let env,v = brick.evaluate this.env
            { this with env = env }, v

        member this.write (brick: 'v brick, value: 'v) =
            { this with newWrites = (brick :> Brick, Some (value :> obj)) :: this.newWrites }

        member this.reset brick = 
            { this with newWrites = (brick, None) :: this.newWrites }

        member this.commitWrites = 
            let commitValue (this:Program) (brick, vo)  =
                let this = this.invalidate [brick]
                let env = this.env
                match vo with
                | None -> this
                | Some v -> {this with env = env.add brick (v, []) }

            let this = this.newWrites |> List.fold commitValue this
            { this with newWrites = [] }

        member this.invalidate (bricks : Brick list) =
            let rec invalidateRec this (bricks : Brick list) = 
                match bricks with
                | [] -> this
                | brick::rest ->
                let hasBrick, (value, trace) = this.env.values.TryGetValue brick
                if not hasBrick then invalidateRec this rest else
                let hasReferrer, referrer = this.env.referrer.TryGetValue brick
                let referrer = if hasReferrer then referrer |> Seq.toList else []
                let this = { this with env = this.env.remove brick }
                invalidateRec this (rest @ referrer)

            bricks |> invalidateRec this

        static member empty = { env = Environment.empty; newWrites = []}


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

    [<CustomOperation("set", MaintainsVariableSpace = true)>]
    member this.Set(nested : Transaction, brick: 'v brick, value: 'v) =
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




