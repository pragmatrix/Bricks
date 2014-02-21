module GCTests

open NUnit.Framework
open FsUnit

open Bricks

[<TestFixture>]
type GCTests() =

    [<Test>]
    member this.GCinvalidatesOrphan() =
        let a = brick { return 3 }

        let b = brick {
            let! a = a
            return a * 3
           }

        let c = brick {
            let! b = b
            return b * 4
        }

        let overwriteB = transaction {
                write b 2
            }

        let p = program {
            let! c1 = c
            c1 |> should equal 36
            apply overwriteB
            let! c2 = c
            c2 |> should equal 8
            let! va1 = valueOf a
            va1 |> should equal (Some 3)
            collect
            let! va2 = valueOf a
            va2 |> should equal (None)
        }

        p Program.empty |> ignore
