module ProcessTests

open Bricks
open NUnit.Framework
open FsUnit


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
        let p = p.invalidate [b]
        p.env.values.ContainsKey b |> should equal false
