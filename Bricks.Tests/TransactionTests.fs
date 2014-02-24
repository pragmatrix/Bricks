module TransactionTests

open NUnit.Framework
open FsUnit

open Bricks

[<TestFixture>]
type TransactionTests() =


    [<Test>]
    member this.transactionSet() =
        
        let a = brick { return 3 }
        let c = brick {
            let! a = a
            return a * 5
        }

        let t = transaction {
            write a 4
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
        
        let p = program {
            let! v = c
            v |> should equal 12
            apply t
            let! v = c
            v |> should equal 15
        }

        p Program.empty |> ignore


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
        
        let p = program {
            let! v = c
            printf "%d\n" v
            v |> should equal 15
            apply t
            let! v = c
            printf "%d\n" v
            v |> should equal 20
        }
        
        p Program.empty |> ignore

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
        
        let p = program {
            let! v = c
            printf "%d\n" v
            v |> should equal 15
            apply t
            let! v = c
            printf "%d\n" v
            v |> should equal 20
            apply rt
            let! v = c
            printf "%d\n" v
            v |> should equal 15
        }

        p Program.empty |> ignore




