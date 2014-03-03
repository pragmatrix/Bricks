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

        let r = c.evaluate()
        r.value |> should equal 15

    [<Test>]
    member this.returnFromCreatesANewBrick() =
        obj.ReferenceEquals(c, d) |> should equal false