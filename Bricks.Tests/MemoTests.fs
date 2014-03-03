module MemoTests

open NUnit.Framework
open FsUnit

open Bricks

[<TestFixture>]
type MemoTests() =

    let a = brick { return 1 }
    let b = brick { 
        let! a = a
        return a * 2 }
    let c = b |> memo 0

    [<Test>]
    member this.simpleMemo() = 

        let t = transaction {
            write a 2
        }

        let p = program {
            let! v = c
            v |> should equal (0, 2)
            apply t
            let! v = c
            v |> should equal (2, 4)
        }

        p Program.empty |> ignore
