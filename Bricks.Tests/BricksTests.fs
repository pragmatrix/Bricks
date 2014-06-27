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
    member this.historyWithThirdPartyEvaluatingSource() =
        let a = value 0
        let b = brick {
            let! ha = historyOf a
            return ha
        }

        let c = brick {
            let! a = a
            return a
        }

        let root = brick {
            let! b = b
            let! c = c
            return (b, c)
        }

        // need to evaluate b once, so that we get a history
        root.evaluate() |> should equal (Reset 0, 0)
        () |> transaction { write a 1 }
        root.evaluate() |> should equal (Progress [1], 1)
        () |> transaction { write a 2 }
        root.evaluate() |> should equal (Progress [2], 2)

    [<Test>]
    member this.yieldWithHistory() = 
        let source = value 0

        let a = brick {
            yield! source
            yield 3
        }

        let root = brick {
            let! ha = historyOf a
            return ha
        }

        root.evaluate() |> should equal (Reset 3)

        () |> transaction { write source 1 }

        root.evaluate() |> should equal (Progress [1;3])


    [<Test>]
    member this.firstTimeHistoryEvaluationReturnsResetLatest() = 
        let source = value 0

        let a = brick {
            yield! source
            yield 3
        }

        let root = brick {
            let! ha = historyOf a
            return ha
        }

        root.evaluate() |> should equal (Reset 3)

    [<Test>]
    member this.conditionalSharedHistoryCatchesUp() = 
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


        let evalC = value true

        let root = brick {
            let! b = b
            let! evalC = evalC
            if (not evalC) then
                return (b, None)
            else
            let! c = c
            return (b, Some c)
        }

        root.evaluate() |> should equal (Reset 3, Some (Reset 3))
        
        () |> transaction { 
            write source 1 
        }

        root.evaluate() |> should equal (Progress [1;3], Some (Progress[1;3]))

        () |> transaction {
            write source 2 
            write evalC false
        }

        let none = Option<History<int>>.None

        let r = root.evaluate() 
        printfn "%A" r
        r |> should equal (Progress [2;3], none)

        () |> transaction {
            write evalC true
        }

        let r = root.evaluate()
        printfn "%A" r
        // the duplicated return of b's Progress [2;3] points to a problem here: historic values
        // should not escape their brick (introduce an elm like foldp?)
        r |> should equal (Progress [2;3], Some (Progress[2;3]))

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

    [<Test>]        
    member this.recursiveEvaluationDoesNotResultInAStackOverflow() = 

        let count = ref 0

        // stack-overflow happens at count 1145, so 10k should be enough
        let max = 10000

        let v : int brick option ref = ref None

        let w = brick {
            count := (!count) + 1
            if !count = max then
                return 0
            else
                let! x = (!v).Value
                return x
        }

        v := 
            brick {
                let! y = w
                return y
            } 
            |> Some


        w.evaluate() |> ignore
        
        


        






