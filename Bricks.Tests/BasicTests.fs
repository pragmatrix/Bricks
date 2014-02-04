module BasicTests

open Bricks

open NUnit.Framework
open FsUnit

[<TestFixture>]
type BasicTests() =
    [<Test>]
    member this.simpleEvaluation() =
        let a = brick {
            return 3 
        }

        let b = brick {
            return 5
        }

        let c = brick {
            let! a = a
            let! b = b
            return a * b
        }

        let ctx = ComputationContext.empty
        let r = c.resolve ctx |> fst
        r |> should equal 15
