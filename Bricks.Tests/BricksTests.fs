module BasicTests

open NUnit.Framework
open FsUnit

open Bricks

[<TestFixture>]
type BrickTests() =

    [<Test>]
    member this.simpleEvaluation() =

        let a = var 3
        let b = var 5
        let c = brick {
            let! a = a
            let! b = b
            return a * b
        }

        let r = c.evaluate()
        r |> should equal 15

    [<Test>]
    member this.dependentEvaluation() =
        let a = var 3
        let b = var 5
        let c = brick {
            let! a = a
            let! b = b
            return a * b
        }

        c.evaluate() |> should equal 15
        () |> transaction { write b 4 }
        c.evaluate() |> should equal 12


    [<Test>]
    member this.sharedEvaluation() =
        let a = var 3
        let b = var 5
        let c = brick {
            let! a = a
            let! b = b
            return a * b
        }
        let d = brick { return! c }

        let e = brick {
            let! c = c
            let! d = d
            return c * d
        }

        e.evaluate() |> should equal 225
        () |> transaction { write a 4 }
        e.evaluate() |> should equal 300

    [<Test>]
    member this.returnFromCreatesANewBrick() =
        let a = var 3
        let b = var 5
        let c = brick {
            let! a = a
            let! b = b
            return a * b
        }
        let d = brick { return! c }


        obj.ReferenceEquals(c, d) |> should equal false

    [<Test>]
    member this.brickSetupIsDelayed() = 
        let x = ref 0
            
        let _ = brick {
            x := 1
            return x
        }

        (!x) |> should equal 0

    [<Test>]        
    member this.recursiveEvaluationDoesNotResultInAStackOverflow() = 

        let count = ref 0

        // stack-overflow happens at count 1145, so 10k should be enough
        let max = 10000

        let v : int brick option ref = ref None

        let w = brick {
            count := (!count) + 1
            if !count = max then
                return 0
            else
                let! x = (!v).Value
                return x
        }

        v := 
            brick {
                let! y = w
                return y
            } 
            |> Some


        w.evaluate() |> ignore
        
        


        






