module Bricks

open System
open System.Diagnostics
open System.Collections.Immutable
open FSharpx.Collections

type HashSet = ImmutableHashSet
type HashSet<'k> = ImmutableHashSet<'k>

type HashMap = ImmutableDictionary
type HashMap<'k, 'v> = ImmutableDictionary<'k, 'v>

(* Brick *)

type Brick = interface end

type Environment = { values: HashMap<Brick, obj * Brick list> }
    with 
        member this.add b v =
            { this with values = this.values.Add(b, v) }
        member this.remove b = 
            { this with values = this.values.Remove b }
        static member empty = 
            { values = ImmutableDictionary.Empty }

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

type BrickBuilder() =
    member this.Bind (dependency: Brick<'dep>, cont: 'dep -> Brick<'next>) : Brick<'next> =
        let f = fun env ->
            let env, depValue = dependency.evaluate env;
            let contBrick = cont depValue
            let env, value = contBrick.evaluate env
            { env = env; trace = [dependency]; value = value }
        Brick<'next>(f)
    member this.Return value = Brick(fun env -> { env = env; trace = []; value = value })

let brick = new BrickBuilder()

(* Program *)

type private ReferrerMap = HashMap<Brick, HashSet<Brick> >

let private addReferrer (map : ReferrerMap) (referrer: Brick * Brick) =
    let key = fst referrer
    let value = snd referrer
    let hasSet, set = map.TryGetValue key
    if hasSet then
        map.Add(key, set.Add value)
    else
        map.Add(key, HashSet.Create value)

let private removeReferrer (map : ReferrerMap) brick referrer = 
    let set = map.[referrer].Remove brick
    map.Add (referrer, set)

let private trace2Referrers trace = 
    let invalidationTarget = fst trace
    snd trace |> List.map (fun dependency -> dependency, invalidationTarget)

module List =
    let flatten l = List.collect id l

type Write = Brick * (obj option)

type Program = 
    { 
        env: Environment; 
        referrer: ReferrerMap; 

        // pending writes
        newWrites: Write list;
    }
    with
        member this.evaluate (brick: Brick<'v>) : Program * 'v = 
            let env,v = brick.evaluate this.env
            { this with env = env }, v

        member this.write (brick: Brick<'v>, value: 'v) =
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
                let hasReferrer, referrer = this.referrer.TryGetValue brick
                let referrer = if hasReferrer then referrer |> Seq.toList else []
                let this = this.removeBrick brick referrer trace
                invalidateRec this (rest @ referrer)

            bricks |> invalidateRec this

        member private this.removeBrick brick referrer trace = 
            let env = brick |> this.env.remove
            let referrer = trace |> List.fold (fun refs dep -> removeReferrer refs dep brick) this.referrer
            let referrer = brick |> referrer.Remove
            { this with env = env; referrer = referrer }

        static member empty = { env = Environment.empty; referrer = HashMap.Empty; newWrites = []}


(* Transaction

    A transaction can evaluate brick values and set new brick values.

    Inside a transaction, evaluating brick values always resolve to the
    previous state of the brick, never to the values that are changed by 
    the transaction.
*)

type Transaction = Program -> Program

type TransactionBuilder() =
    member this.Bind (brick: Brick<'value>, cont: 'value -> Transaction) : Transaction = 
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
        printf "t:for %A\n" seq
        fun p ->
            let p = seq p
            cont() p

    [<CustomOperation("set", MaintainsVariableSpace = true)>]
    member this.Set(nested : Transaction, brick: Brick<'v>, value: 'v) =
        printf "t:set\n"
        fun (p: Program) ->
            let p = nested p
            printf "t:calling write %A\n" value
            p.write (brick, value)
            
    [<CustomOperation("reset", MaintainsVariableSpace = true)>]
    member this.Reset(nested: Transaction, brick : Brick) =
        printf "t:reset\n"
        fun (p: Program) ->
            let p = nested p
            printf "t:calling reset\n"
            p.reset brick

let transaction = new TransactionBuilder()

type ProgramM = Program -> Program

type ProgramBuilder() =

    (* A regular let! is the isolated evaluation of a root brick in the context of the process *)

    member this.Bind (brick: Brick<'value>, cont: 'value -> ProgramM) : ProgramM = 
        printf "bind %A\n" brick
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
        printf "apply\n"
        fun p ->
            let p = nested p
            transaction p

let program = new ProgramBuilder()


