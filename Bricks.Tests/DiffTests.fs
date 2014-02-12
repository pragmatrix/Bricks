module DiffTests

open NUnit.Framework
open FsUnit

open Bricks

type DiffTests() =
    
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
