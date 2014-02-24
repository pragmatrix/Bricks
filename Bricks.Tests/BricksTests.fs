module BasicTests

open NUnit.Framework
open FsUnit

open Bricks

[<TestFixture>]
type BrickTests() =

    let a = brick { return 3 }
    let b = brick { return 5 }
    let c = brick {
        let! a = a
        let! b = b
        return a * b
    }
    let d = brick { return! c }

    [<Test>]
    member this.simpleEvaluation() =

        let env = Environment.empty
        let _,r = c.evaluate env
        r |> should equal 15

    [<Test>]
    member this.collectsTraces() = 
        let env = Environment.empty
        let env,_ = c.evaluate env
        printf "%A" env
        // note that c itself is not dependent on a / b directly but on an internal continuation brick.
        env.values.[c].trace.Length |> should equal 2

    [<Test>]
    member this.returnFromCreatesANewBrick() =
        obj.ReferenceEquals(c, d) |> should equal false