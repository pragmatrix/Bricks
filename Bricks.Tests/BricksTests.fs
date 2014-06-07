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
        r |> should equal 15

    [<Test>]
    member this.returnFromCreatesANewBrick() =
        obj.ReferenceEquals(c, d) |> should equal false


    [<Test>]
    member this.historyDoesNotExistOnNewValue() =
        let a = value 0
        let b = brick {
            let! ha = historyOf a
            return ha
        }

        b.evaluate() |> should equal (Reset 0)

    [<Test>]
    member this.historyDoesNotExistIfValueWasChangedButNotEvaluatedBefore() =
        let a = value 0
        () |> transaction { write a 1 }
        let b = brick {
            let! ha = historyOf a
            return ha
        }

        b.evaluate() |> should equal (Reset 1)

    [<Test>]
    member this.historyExistsIfValueWasEvaluated() =
        let a = value 0
        let b = brick {
            let! ha = historyOf a
            return ha
        }

        b.evaluate() |> should equal (Reset 0)

        () |> transaction { write a 1 }

        b.evaluate() |> should equal (History [1])

        () |> transaction { write a 2 }

        b.evaluate() |> should equal (History [2])


    [<Test>]
    member this.historyWithThirdParty() =
        let a = value 0
        let b = brick {
            let! ha = historyOf a
            return ha
        }

        let c = brick {
            let! a = a
            return a
        }

        // need to evaluate b once, so that we get a history
        b.evaluate() |> ignore
        c.evaluate() |> ignore
        () |> transaction { write a 1 }
        c.evaluate() |> ignore
        () |> transaction { write a 2 }
        c.evaluate() |> ignore
        b.evaluate() |> should equal (History [1;2])

    [<Test>]
    member this.yieldWithHistory() = 
        let source = value 0

        let a = brick {
            yield! source
            yield 3
        }

        let b = brick {
            let! ha = historyOf a
            return ha
        }

        b.evaluate() |> should equal (Reset 3)

        () |> transaction { write source 1 }

        b.evaluate() |> should equal (History [1;3])

        







