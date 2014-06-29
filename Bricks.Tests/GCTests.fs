module GCTests

open NUnit.Framework
open FsUnit

open Bricks
open System

[<TestFixture>]
type GCTests() =


(*
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

        c.evaluate() |> should equal 36
        () |> transaction { write b 2 }
        c.evaluate() |> should equal 8
        valueOf a |> should equal None
*)

    [<Test>]
    member this.GCDoesNotInvalidateOrphanByNotEvaluating() =
        let a = lift 3

        let useA = lift true

        let c = brick {
            let! useA = useA
            if useA then
                return! a
            else
                return 0
           }

        c.evaluate() |> should equal 3
        () |> transaction { write useA false }
        c.evaluate() |> should equal 0
        valueOf a |> should equal (Some 3)

(*
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

        c.evaluate() |> should equal 27
        () |> transaction { write b 2 }
        c.evaluate() |> should equal 6
        valueOf a |> should equal (Some 3)
*)

    [<Test>]
    member this.NativeGCCollectsTemporarilyUsedBrick() = 
        let a = brick { return 3 }

        let newTmp() = brick {
            let! a = a
            return a * 3
            }

        let run() = 
            let tmp = newTmp()

            use p = tmp |> toProgram
            p.run() |> should equal 9

            WeakReference(tmp)

        let weakTemp = run()

        // now the program can be collected
        GC.Collect(GC.MaxGeneration, GCCollectionMode.Forced, true)
        weakTemp.IsAlive |> should equal false

        // be sure a stays in memory.
        // (gc may detect that they are not anymore used before the Weak tests above)
        printf "%A" a

    [<Test>]
    member this.NativeGCCollectsTemporarilyUsedBrickFromRunBefore() = 
        let a = brick { return 3 }

        let newTmp(mul) = brick {
            let! a = a
            return a * mul
            }

        let runProgram() = 
            let tmps = [| newTmp(0); newTmp(1) |]
            let current = lift 0
            let b = brick {
                let! c = current
                let! tmp = tmps.[c]
                printf "tmp: %A" tmp
                return tmp
            }
            use p = toProgram b
            p.run() |> should equal 0
            () |> transaction {write current 1}
            p.run() |> should equal 3
            WeakReference(tmps.[0]), WeakReference(tmps.[1])

        let weakTemp, weakTemp2 = runProgram()

        // now the program can be collected
        GC.Collect(GC.MaxGeneration, GCCollectionMode.Forced, true)
        weakTemp.IsAlive |> should equal false
        weakTemp2.IsAlive |> should equal false

        // be sure a stays in memory.
        // (gc may detect that they are not anymore used before the Weak tests above)
        printf "%A" a

    [<Test>]
    member this.GCCollectedWritesTranscend() =
        let alive = lift true
        let a = lift 1
        let b = brick {
            let! alive = alive
            if (alive) then
                let! a = a
                return Some a
            else
                return None
        }

        let p = b |> toProgram

        p.run() |> should equal (Some 1)

        p.apply (transaction { write a 2 })
        p.run() |> should equal (Some 2)

        p.apply (transaction { write alive false })
        p.run() |> should equal None

        p.apply (transaction {write alive true })
        p.run() |> should equal (Some 2)
        

