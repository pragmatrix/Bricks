module TransactionTests

open NUnit.Framework
open FsUnit

open Bricks

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
            set a 4
        }
        
        let p = program {
            let! v = c
            v |> should equal 15
            apply t
            let! v = c
            v |> should equal 20
        }

        p Program.empty |> ignore


    [<Test>]
    member this.transactionReset() =
        let t = transaction {
            set a 4
        }

        let rt = transaction {
            reset a
        }
        
        let p = program {
            let! v = c
            v |> should equal 15
            apply t
            let! v = c
            v |> should equal 20
            apply rt
            let! v = c
            v |> should equal 15
        }

        p Program.empty |> ignore




