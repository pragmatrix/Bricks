module TransactionTests

open NUnit.Framework
open FsUnit

open Bricks
open BrickTests

[<TestFixture>]
type TransactionTests() as this =
    inherit BrickTestBase()

    let evaluate b = this.eval b

    [<Test>]
    member this.transactionWrite() =
        
        let a = brick { return 3 }
        let c = brick {
            let! a = a
            return a * 5
        }

        let t = transaction {
            write a 4
        }
        
        evaluate c |> should equal 15
        t()
        evaluate c |> should equal 20

    [<Test>]
    member this.continuationGetsInvalidated() = 
        let a = brick { return 3 }
        let b = brick { return 4 }
        let c = brick {
            let! a = a
            let! b = b
            return a * b
        }

        let t = transaction {
            write b 5
        }

        evaluate c |> should equal 12
        t()
        evaluate c |> should equal 15

    [<Test>]
    member this.lastWriteWins() =
        let a = brick { return 3 }
        let c = brick {
            let! a = a
            return a * 5
        }

        let t = transaction {
            write a 3
            write a 4
        }

        evaluate c |> should equal 15
        t()
        evaluate c |> should equal 20

    [<Test>]
    member this.transactionReset() =
        let a = brick { return 3 }
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
  
        evaluate c |> should equal 15
        t()
        evaluate c |> should equal 20
        rt()
        evaluate c |> should equal 15
        
    [<Test>]
    member this.aWriteCanNotBeInvalidated() =
        let a = brick { return 3 }
        let b = brick { 
            let! a = a 
            return a * 2}

        evaluate b |> should equal 6
        (transaction { write b 4 })()
        evaluate b |> should equal 4
        (transaction { write a 0 })()
        evaluate b |> should equal 4
