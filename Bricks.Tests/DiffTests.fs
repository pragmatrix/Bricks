module DiffTests

open NUnit.Framework
open FsUnit

open Bricks

(*
type DiffTests() =
    
    let a = brick { return set.empty }
    let b = brick { return! a }

    [<Test>]
    member this.transactionSet() =
        
        let t = transaction {
            write a (set.ofSeq [1;2;3;4])
        }
        
        let p = program {
            let! v = c
            v |> should equal 15
            apply t
            let! v = c
            v |> should equal 20
        }

        p Program.empty |> ignore
*)
