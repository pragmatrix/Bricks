﻿module ProgramTests

open NUnit.Framework
open FsUnit

open Bricks

[<TestFixture>]
type ProgramTests() =

    [<Test>]
    member this.letMayBeUsedToAddAdditionalRoots() =
        let ba = value 10
        let bb = value 20

        let p = program {
            let! b = ba
            printf "%A" b
            let! _ = [bb]
            let! b = ba
            printf "%A" b
        }

        p.run()
        let roots = p.roots
        roots |> should equal [ba; bb; ba]