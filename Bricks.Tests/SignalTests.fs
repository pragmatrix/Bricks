﻿module SignalTests

open NUnit.Framework
open FsUnit

[<TestFixture>]
type SignalTests() =

    [<Test>]
    member this.simpleSignalProcessing() = 
        let source = var 1
        let target = Signal.map ((*) 2) source
        
        target.evaluate() |> should equal 2

    [<Test>]
    member this.signalWritesInTheSameTransactionGetCombined() = 
        let source = var 1
        let source2 = var 2

        let count = ref 0

        let f a b = 
            count := !count + 1
            a * b

        let target = Signal.lift2 f source source2

        target.evaluate() |> should equal 2

        !count |> should equal 1

        () |> transaction {
            write source 2 
            write source2 3
            }

        target.evaluate() |> should equal (2 * 3)
        !count |> should equal 2


    (* it might be better to allow only one write, the latest write in the transaction *)

    [<Test>]
    member this.duplicatedWritesToTheSameSignalInTheSameTransactionGetSeparated() = 
        let source = var 1
        let source2 = var 2

        let count = ref 0

        let f a b = 
            count := !count + 1
            a * b

        let target = Signal.lift2 f source source2

        target.evaluate() |> should equal 2

        !count |> should equal 1

        () |> transaction {
            write source 2 
            write source2 3
            write source 2
            }

        target.evaluate() |> should equal (2 * 3)
        printfn "%A" count
        !count |> should equal 3

    [<Test>]
    member this.signalWritesInADifferentTransactionGetSeparated() = 
        let source = var 1
        let source2 = var 2

        let count = ref 0

        let f a b = 
            count := !count + 1
            a * b

        let target = Signal.lift2 f source source2

        target.evaluate() |> should equal 2

        !count |> should equal 1

        () |> transaction {
            write source 2 
            }

        () |> transaction {
            write source2 3
        }

        target.evaluate() |> should equal (2 * 3)
        !count |> should equal 3

    (* The following behavior is by design so far, in the first evaluation, 
       only the latest value is processed, subsequent evaluations process all *)

    [<Test>]
    member this.foldp1IgnoreFirstWriteInFirstEvaluation() = 
        let source = var 1
        () |> transaction { write source 2 }
        
        let target = Signal.foldp (+) 0 source

        target.evaluate() |> should equal 2


    [<Test>]
    member this.foldp1DoesNotLooseValuesAfterFirstEvaluation() = 
        let source = var 1
        
        let target = Signal.foldp (+) 0 source

        target.evaluate() |> should equal 1

        () |> transaction { write source 2 }
        () |> transaction { write source 3 }

        target.evaluate() |> should equal (1 + 2 + 3)

        







