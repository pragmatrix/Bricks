module DiffTests

open NUnit.Framework
open FsUnit

open Bricks

[<TestFixture>]
type MemoTests() =


    [<Test>]
    member this.simpleMemo() = 

        let a = brick { return 1 }
        let b = brick { 
            let! a = a
            return a * 2 }
        let c = memo b 0

        let t = transaction {
            write a 2
        }

        let p = program {
            let! v = c
            v |> should equal (0, 2)
            apply t
            let! v = c
            v |> should equal (2, 4)
        }

        p Program.empty |> ignore
(*
    
    let a = brick { return set.empty }
    let b = brick {
            let! x = diff a 
            return x }

    [<Test>]
    member this.transactionSet() =
        
        let t = transaction {
            write a (set.ofSeq [1;2;3;4])
        }
        
        let p = program {
            let! v = b
            v |> should equal []
            apply t
            let! v = c
            v |> should equal 20
        }

        p Program.empty |> ignore
*)