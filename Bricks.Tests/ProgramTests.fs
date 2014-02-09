module ProcessTests

open Bricks
open NUnit.Framework
open FsUnit


(*


[<TestFixture>]
type ProcessTests() =

    let a = brick { return 3 }
    let b = brick { return 5 }
    let cb = brick {
        let! a = a
        let! b = b
        return a * b
    }

    [<Test>]
    member this.invalidationTest() =
        let p = program {
            let! cv = cb
            printf "%A" cv
            set cb 4
        }

        p Program.empty
        true |> should equal true

*)
