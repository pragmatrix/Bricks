module TransactionTests

open Bricks
open NUnit.Framework
open FsUnit


[<TestFixture>]
type ProcessTests() =

    let a = brick { return 3 }
    let b = brick { return 5 }
    let c = brick {
        let! a = a
        let! b = b
        return a * b
    }

    [<Test>]
    member this.transactionSet() =
        
        let t = transaction {
            printf "transaction: before set\n"
            set a 2
            printf "transaction: inbetween set\n"
            set a 4
            printf "transaction: after set\n"
        }
        
        let p = program {
            let! v = c
            v |> should equal 15
            apply t
            apply t
            let! v = c
            v |> should equal 20
        }



        p Program.empty |> ignore



