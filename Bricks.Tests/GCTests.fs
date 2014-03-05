module GCTests

open NUnit.Framework
open FsUnit

open Bricks

[<TestFixture>]
type GCTests() =

    [<Test>]
    member this.GCInvalidatesOrphanByWrite() =
        let a = brick { return 3 }

        let b = brick {
            let! a = a
            return a * 3
           }

        let c = brick {
            let! b = b
            return b * 4
        }

        let p = program {
            let! c1 = c
            c1 |> should equal 36
            apply (transaction { write b 2 })
            let! c2 = c
            c2 |> should equal 8
            let! va1 = valueOf a
            va1 |> should equal None
        }

        p Program.empty |> ignore

    [<Test>]
    member this.GCInvalidatesOrphanByNotEvaluating() =
        let a = brick { return 3 }

        let useA = brick { return true }

        let c = brick {
            let! useA = useA
            if useA then
                return! a
            else
                return 0
           }

        let p = program {
            let! c1 = c
            c1 |> should equal 3
            apply (transaction { write useA false })
            let! c2 = c
            c2 |> should equal 0
            let! va1 = valueOf a
            va1 |> should equal None
        }

        p Program.empty |> ignore

    [<Test>]
    member this.GCDoesNotInvalidateSharedDependency() =
        let a = brick { return 3 }

        let b = brick {
            let! a = a
            return a * 3
           }

        let c = brick {
            let! b = b
            let! a = a
            return b * a
        }

        let p = program {
            let! c1 = c
            c1 |> should equal 27
            apply (transaction { write b 2 } )
            let! c2 = c
            c2 |> should equal 6
            let! va1 = valueOf a
            va1 |> should equal (Some 3)
        }

        p Program.empty |> ignore
