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

        b.evaluate() |> should equal (Progress [1])

        () |> transaction { write a 2 }

        b.evaluate() |> should equal (Progress [2])


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
        b.evaluate() |> should equal (Progress [1;2])

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

        b.evaluate() |> should equal (Progress [1;3])

    [<Test>]
    member this.sharedHistory() = 
        let source = value 0

        let a = brick {
            yield! source
            yield 3
        }

        let b = brick {
            let! ha = historyOf a
            return ha
        }

         let c = brick {
            let! ha = historyOf a
            return ha
        }

        b.evaluate() |> should equal (Reset 3)
        c.evaluate() |> should equal (Reset 3)

        () |> transaction { write source 1 }

        b.evaluate() |> should equal (Progress [1;3])

        () |> transaction { write source 2 }

        b.evaluate() |> should equal (Progress [2;3])
        c.evaluate() |> should equal (Progress [1;3;2;3])

    [<Test>]
    member this.previous() =

        let source = value 0

        let a = brick {
            let! p = previousOf source
            let! s = source
            return (p, s)
        }

        let none = option<int>.None

        a.evaluate() |> should equal (none, 0)

        () |> transaction { write source 1 }

        a.evaluate() |> should equal (Some 0, 1)
      
        () |> transaction { write source 2 }
        () |> transaction { write source 3 }
        
        a.evaluate() |> should equal (Some 1, 3)

    [<Test>]
    member this.previousGetsLostIfSourceIsNotReferencedAnymore() =
        
        let source = value 0
        let useSource = value true

        let a = brick {
            let! p = previousOf source
            let! us = useSource
            if (us) then
                let! s = source
                return (p, Some s)
            else
                return (p, None)
        }

        let none = option<int>.None

        a.evaluate() |> should equal (none, Some 0)

        () |> transaction { write source 1 }

        a.evaluate() |> should equal (Some 0, Some 1)

        () |> transaction { write useSource false }

        a.evaluate() |> should equal (Some 1, none)

        // source is not anymore referenced!
        () |> transaction { write source 2 }

        // but previous exists in the next run, even though we don't access source animore.
        a.evaluate() |> should equal (Some 1, none)

        // and even though we switch source now on, the previous is lost now
        () |> transaction { write useSource true }
        a.evaluate() |> should equal (none, Some 2)


    [<Test>]
    member this.brickSetupIsDelayed() = 
        let x = ref 0
            
        let _ = brick {
            x := 1
            return x
        }

        (!x) |> should equal 0

(*
    [<Test>]        
    member this.recursiveEvaluationDoesNotResultInAStackOverflow() = 

        let v : int brick option ref = ref None

        let w = brick {
            let! x = (!v).Value
            return x * 2
        }

        v := 
            brick {
                let! y = w
                return y * 2
            } 
            |> Some

*)
        
        


        






