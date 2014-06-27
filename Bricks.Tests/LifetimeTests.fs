module LifetimeTests

open NUnit.Framework
open FsUnit

open Bricks
open System

type Instance() = 
    [<DefaultValue>] val mutable Disposed : bool
    [<DefaultValue>] val mutable Value : int
    
    interface IDisposable with
        member this.Dispose() = this.Disposed <- true

(*

[<TestFixture>]
type LifetimeTests() =

    [<Test>]
    member this.testInstanceDestruction() =
        let instance = new Instance()
        let a = value 1
        let i = brick {
            let! i = manifest (fun () -> instance)
            let! a = a
            i.Value <- a
            return i
        }

        let show = value true

        let b = brick {
            let! s = show
            if (s) then
                let! i = i
                return Some i
            else return None
        }

        let p = toProgram b

        // initial run

        p.run() |> ignore
        instance.Value |> should equal 1
        instance.Disposed |> should equal false

        // change a to 2

        let changeA = transaction { write a 2 }
        p.apply changeA
        p.run() |> ignore
        instance.Value |> should equal 2

        // GC collect instance
        let t = transaction { write show false }

        p.apply t
        p.run() |> ignore
        instance.Disposed |> should equal true

        // Reincarnate instance
        instance.Disposed <- false
        let t = transaction { write show true }
        p.apply t
        p.run() |> ignore
        instance.Disposed |> should equal false
        instance.Value |> should equal 2

*)
