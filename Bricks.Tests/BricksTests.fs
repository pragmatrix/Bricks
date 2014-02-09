module BasicTests

open Bricks

open NUnit.Framework
open FsUnit

[<TestFixture>]
type BrickTests() =

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
    member this.collectsTraces() = 
        let ctx = ComputationContext.empty
        let ctx = c.resolve ctx |> snd
        printf "%A" ctx
        // note that c itself is not dependent on a / b directly.
        ctx.traces |> List.map snd |> should equal [[a]; [b]]

    [<Test>]
    member this.noMoreTracesOnSecondEvaluation() =
        let ctx = ComputationContext.empty
        let ctxb = c.resolve ctx |> snd
        let ctxc = c.resolve ctxb |> snd
        printf "%A" ctx
        // note that c itself is not dependent on a / b directly.
        ctxc.traces |> should equal ctxb.traces

(*
    [<Test>]
    member this.inputChanged() =
        let ctx = ComputationContext.empty
        let r = c.resolve ctx
        r |> ignore

*)



