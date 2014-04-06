module GCTests

open NUnit.Framework
open FsUnit

open Bricks
open System

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

        p.run()

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

        p.run()

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

        p.run()

    [<Test>]
    member this.NativeGCCollectsTemporarilyUsedBrick() = 
        let a = brick { return 3 }

        let newTmp() = brick {
            let! a = a
            return a * 3
            }

        let runProgram() = 
            let tmp = newTmp()

            use p = program {
                // be sure a and b are evaluated
                let! tmp = tmp
                tmp |> should equal 9
            }

            p.run()
            WeakReference(tmp)

        let weakTemp = runProgram()

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
            let current = brick { return 0; }
            use p = program {
                let! c = current
                let! tmp = tmps.[c]
                printf "tmp: %A" tmp
                tmp |> should equal (3 * c)
            }

            p.run()
            p.apply(transaction {write current 1})
            p.run()
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
        let alive = value true
        let a = value 1
        let latest = ref 0
        let p = program {
            let! alive = alive
            if (alive) then
                let! a = a
                latest := a
        }

        p.run()
        !latest |> should equal 1


        p.apply (transaction { write a 2 })
        p.run()
        !latest |> should equal 2


        latest := 0
        p.apply (transaction { write alive false })
        p.run()
        !latest |> should equal 0

        p.apply (transaction {write alive true })
        p.run()
        !latest |> should equal 2

        


