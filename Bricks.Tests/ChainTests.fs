namespace Jungle.Processes.Tests

open NUnit.Framework
open FsUnit
open Chain

[<TestFixture>]
type ChainTests() = 

    [<Test>]
    member this.push() =
        let ch = Chain.single(0)
        let consumer1 = ch
        ch.push 1 |> ignore
        consumer1.all() |> should equal [0; 1]

    [<Test>]
    member this.push2() =
        let ch = Chain.single(0)
        let consumer1 = ch
        let producer = ch.push 1
        producer.push 2 |> ignore
        consumer1.all() |> should equal [0; 1; 2]

    [<Test>]
    member this.chainInside() =
        let ch = Chain.single(0)
        let consumer1 = ch
        let producer = ch.push 1
        let cons = consumer1.all() 
        cons |> Seq.toArray |> should equal [0;1]
        let cons = producer.push 2
        cons.all() |> should equal [2]
        
    // note: range is inclusive, this is because there can not be empty segments.

    [<Test>]
    member this.range1() =
        let ch = Chain.single(0)
        let r = Chain.range ch ch
        Seq.toList r |> should equal [0]

    [<Test>]
    member this.range2() =
        let ch = Chain.single(0)
        let e = ch.push 1
        let r = Chain.range ch e
        Seq.toList r |> should equal [0; 1]

    [<Test>]
    member this.rangePartial() =
        let ch = Chain.single(0)
        let e1 = ch.push 1
        let _ = e1.push 2
        let r = range ch e1
        Seq.toList r |> should equal [0; 1]

    [<Test>]
    [<ExpectedException(typedefof<AlienChainError>)>]
    member this.rangeAlien() =
        let ch = Chain.single(0)
        let ch2 = Chain.single(0)
        let r = range ch ch2
        Seq.toList r |> ignore


    [<Test>]
    member this.ofSeq() =
        let ch = Chain.ofSeq([0;1;2])
        ch.all() |> should equal [0;1;2]


