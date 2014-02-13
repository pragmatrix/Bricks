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
        let p,_ = p.evaluate b
        p.env.values.ContainsKey b |> should equal true
        let p = { p with env = p.env.invalidate [b]}
        p.env.values.ContainsKey b |> should equal false
