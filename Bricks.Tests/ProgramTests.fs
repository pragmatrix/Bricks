module ProcessTests

open NUnit.Framework
open FsUnit

open Bricks

[<TestFixture>]
type ProgramTests() =

    [<Test>]
    member this.properlyInvalidatesBrick() =
        let b = brick {
            return 3;
        }
        let p = Program.empty
        let _ = p.evaluate b
        b.Value.IsSome |> should equal true
        let p = p.invalidate [b]
        b.Value.IsSome |> should equal false
