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

type Environment = { values: HashMap<Brick, obj> }
    with 
        member this.add b v =
            { this with values = this.values.Add(b, v) }
        member this.remove b = 
            { this with values = this.values.Remove b }
        static member empty = 
            { values = ImmutableDictionary.Empty }

type Trace = Brick * Brick list

type ComputationContext = { env: Environment; traces: Trace list }
    with 
        static member empty = { env = Environment.empty; traces = []}
        static member fromEnv env = { env = env; traces = []}

and ComputationResult<'v> = 'v * ComputationContext

type Computation<'v> = ComputationContext -> ComputationResult<'v>

type Brick<'v>(f : Computation<'v>) =
    interface Brick
    with 
        member this.resolve ctx = 
            let values = ctx.env.values
            if values.ContainsKey this then
                values.[this] :?>'v, ctx
            else
            let v, ctx = f ctx
            v, { ctx with env = ctx.env.add this v }

type BrickBuilder() =
    member this.Bind (dependency: Brick<'dep>, cont: 'dep -> Brick<'next>) : Brick<'next> =
        let f = fun ctx ->
            let v, ctx = dependency.resolve ctx;
            let contBrick = cont v
            let v, ctx = contBrick.resolve ctx
            v, { ctx with traces = (contBrick :> Brick, [ dependency ]) :: ctx.traces }
        Brick<'next>(f)
    member this.Return value = Brick(fun ctx -> value, ctx)

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

let private flattenList l = List.collect id l

let private addTrace (map: HashMap<Brick, Brick list>) (trace: Trace) =
    map.Add(fst trace, snd trace)

type Program = 
    { 
        env: Environment; 
        traces: HashMap<Brick, Brick list>
        referrer: ReferrerMap; 

        // pending traces
        newTraces: Trace list list;
    }
    with
        member this.set (brick: Brick<'v>, value: 'v) =
            let this = this.invalidate brick
            let env = this.env.add brick value
            { this with env = env }

        member this.invalidate (brick : Brick) = 
            this.invalidate [brick]

        member this.invalidate (bricks : Brick list) =
            let rec invalidateRec program (bricks : Brick list) = 
                match bricks with
                | [] -> program
                | brick::rest ->
                let hasBrick, brickTrace = program.traces.TryGetValue brick
                if not hasBrick then invalidateRec program rest else
                let referrer = this.referrer.[brick] |> Seq.toList
                let this = this.removeBrick brick referrer brickTrace
                invalidateRec this (rest @ referrer)

            let this = this.integrateTraces
            bricks |> invalidateRec this

        member private this.integrateTraces = 
            if this.newTraces = [] then this else
            let newTraces = this.newTraces |> flattenList
            let traces = newTraces |> List.fold addTrace this.traces
            let referrer = newTraces |> List.map trace2Referrers |> flattenList
            let referrer = referrer |> List.fold addReferrer this.referrer
            { this with referrer = referrer; traces = traces; newTraces = [] }

        member private this.removeBrick brick referrer trace = 
            assert (this.newTraces = [])
            let env = brick |> this.env.remove
            let traces = brick |> this.traces.Remove
            let referrer = trace |> List.fold (fun refs dep -> removeReferrer refs dep brick) this.referrer
            let referrer = brick |> referrer.Remove
            { this with env = env; traces = traces; referrer = referrer }

        static member empty = { env = Environment.empty; traces = HashMap.Empty; referrer = HashMap.Empty; newTraces = []}


(* Transaction

    A transaction can evaluate brick values and set new brick values.

    Inside a transaction, evaluating brick values always resolve to the
    previous state of the brick, never to the reality of the values that are
    changed by the transaction.
*)

type TransactionBuilder() =
    member this.Bind (brick: Brick<'value>, cont: 'value -> ProgramM) : ProgramM = 
        fun p ->
            let ctx = ComputationContext.fromEnv p.env
            let v, ctx = brick.resolve ctx
            let newTraces = ctx.traces
            let p = { p with newTraces = newTraces :: p.newTraces}
            cont v p

    member this.Zero () = id
    member this.Yield _ = id

    [<CustomOperation("set", MaintainsVariableSpace = true)>]
    member this.Set(nested : ProgramM, brick: Brick<'v>, value: 'v) =
        fun (p: Program) ->
            p.set (brick, value)
            
and ProgramM = Program -> Program

type private PPAttribute = ProjectionParameterAttribute

type ProgramBuilder() =
    (* right now we do not support integrating other processes *)
    (* regular Bind is the isolated evaluation of a root brick in the context of the process *)

    member this.Bind (brick: Brick<'value>, cont: 'value -> ProgramM) : ProgramM = 
        fun p ->
            let ctx = ComputationContext.fromEnv p.env
            let v, ctx = brick.resolve ctx
            let newTraces = ctx.traces
            let p = { p with newTraces = newTraces :: p.newTraces}
            cont v p

    member this.Zero () = id


let program = new ProgramBuilder()


