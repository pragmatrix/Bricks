﻿module BasicTests

open Bricks

open NUnit.Framework
open FsUnit

[<TestFixture>]
type BrickTests() =

    let a = brick { return 3 }
    let b = brick { return 5 }
    let c = brick {
        let! a = a
        let! b = b
        return a * b
    }

    [<Test>]
    member this.simpleEvaluation() =

        let env = Environment.empty
        let _,r = c.evaluate env
        r |> should equal 15

    [<Test>]
    member this.collectsTraces() = 
        let env = Environment.empty
        let env,_ = c.evaluate env
        printf "%A" env
        // note that c itself is not dependent on a / b directly but on an internal continuation brick.
        env.values.[c] |> snd |> fun a -> a.Length |> should equal 1
