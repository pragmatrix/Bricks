module BasicTests

open Bricks

open NUnit.Framework
open FsUnit

[<TestFixture>]
type BasicTests() =

    let a = brick { return 3 }
    let b = brick { return 5 }
    let c = brick {
        let! a = a
        let! b = b
        return a * b
    }

    [<Test>]
    member this.simpleEvaluation() =

        let ctx = ComputationContext.empty
        let r = c.resolve ctx |> fst
        r |> should equal 15

    [<Test>]
    member this.tracksDependencies() = 
        let ctx = ComputationContext.empty
        let ctx = c.resolve ctx |> snd
        printf "%A" ctx
        // note that c itself is not dependent on a / b directly.
        ctx.newDeps |> List.map snd |> should equal [a; b]

(*
    [<Test>]
    member this.inputChanged() =
        let ctx = ComputationContext.empty
        let r = c.resolve ctx
        r |> ignore

*)



