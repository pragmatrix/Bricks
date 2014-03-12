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
            WeakReference(p), WeakReference(tmp)

        let weakProgram, weakTemp = runProgram()

        // now the program can be collected
        GC.Collect(GC.MaxGeneration, GCCollectionMode.Forced, true)
        weakProgram.IsAlive |> should equal false
        weakTemp.IsAlive |> should equal false

        // be sure a stays in memory.
        // (gc may detect that they are not anymore used before the Weak tests above)
        printf "%A" a


