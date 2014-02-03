namespace Bricks

open System
open FSharpx.Collections


type HashMap<'k, 'v when 'k : equality and 'v : equality> = PersistentHashMap<'k, 'v>

type Brick = interface end

type Environment = { values: HashMap<Brick, obj> }
    with 
        member this.add b v =
            { this with values = this.values.Add(b, v) }

type Dependency = Brick * Brick

type ComputationContext = { env: Environment; newDeps: Dependency list }

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

type BrickBuilder<'v> =
    member this.Bind (dependency: Brick<'dep>, cont: 'dep -> Brick<'next>) : Brick<'next> =
        let f = fun ctx ->
            let v, ctx = dependency.resolve ctx;
            let contBrick = cont v
            let v, ctx = contBrick.resolve ctx
            v, { ctx with newDeps = (contBrick :> Brick, dependency :> Brick) :: ctx.newDeps }
        Brick<'next>(f)
    