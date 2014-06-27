module TransactionTests

open NUnit.Framework
open FsUnit

open Bricks

[<TestFixture>]
type TransactionTests() =


    [<Test>]
    member this.transactionWrite() =
        
        let a = value 3
        let c = brick {
            let! a = a
            return a * 5
        }

        let t = transaction {
            write a 4
        }
        
        c.evaluate() |> should equal 15
        t()
        c.evaluate() |> should equal 20

    [<Test>]
    member this.continuationGetsInvalidated() = 
        let a = value 3
        let b = value 4
        let c = brick {
            let! a = a
            let! b = b
            return a * b
        }

        let t = transaction {
            write b 5
        }

        c.evaluate() |> should equal 12
        t()
        c.evaluate() |> should equal 15

    [<Test>]
    member this.lastWriteWins() =
        let a = value 3
        let c = brick {
            let! a = a
            return a * 5
        }

        let t = transaction {
            write a 3
            write a 4
        }

        c.evaluate() |> should equal 15
        t()
        c.evaluate() |> should equal 20

    [<Test>]
    member this.transactionReset() =
        let a = value 3
        let c = brick {
            let! a = a
            return a * 5
        }

        let t = transaction {
            write a 4
        }

        let rt = transaction {
            reset a
        }
  
        c.evaluate() |> should equal 15
        t()
        c.evaluate() |> should equal 20
        rt()
        c.evaluate() |> should equal 15

(*        
    [<Test>]
    member this.aWriteCanNotBeInvalidated() =
        let a = brick { return 3 }
        let b = brick { 
            let! a = a 
            return a * 2}

        b.evaluate() |> should equal 6
        (transaction { write b 4 })()
        b.evaluate() |> should equal 4
        (transaction { write a 0 })()
        b.evaluate() |> should equal 4
*)
