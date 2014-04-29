namespace Jungle.Processes.Tests

open NUnit.Framework
open FsUnit
open Chain

[<TestFixture>]
type ChainTests() = 

    [<Test>]
    member this.produceConsumeTest() =
        let ch = Chain.empty()
        let consumer1 = ch
        ch.push 1 |> ignore
        consumer1.consumeAll() |> fst |> should equal [1]

    [<Test>]
    member this.doubleProduceConsumeTest() =
        let ch = Chain.empty()
        let consumer1 = ch
        let producer = ch.push 1
        producer.push 2 |> ignore
        consumer1.consumeAll() |> fst |> should equal [1; 2]

    [<Test>]
    member this.doubleProduceDoubleConsumeTest() =
        let ch = Chain.empty()
        let consumer1 = ch
        let producer = ch.push 1
        let cons = consumer1.consumeAll() 
        cons |> fst |> Seq.toArray |> should equal [1]
        producer.push 2 |> ignore
        cons |> snd |> fun c -> c.consumeAll() |> fst |> should equal [2]

    [<Test>]
    member this.twoConsumersAtDifferentPositions() =
        let ch = Chain.empty()
        let consumer1 = ch
        let consumer2 = ch
        let producer = ch.push 1
        let cons = consumer1.consumeAll() 
        cons |> fst |> Seq.toArray |> should equal [1]
        producer.push 2 |> ignore
        cons |> snd |> fun c -> c.consumeAll() |> fst |> should equal [2]
        consumer2.consumeAll() |> fst |> should equal [1;2]

    [<Test>]
    member this.ifNothingConsumedConsumeAllReturnsEmpty() =
        let ch = Chain.empty()
        let consumer1 = ch
        consumer1.consumeAll() |> fst |> should equal []

    [<Test>]
    member this.ifAllConsumedConsumeAllReturnsEmpty() =
        let ch = Chain.empty()
        ch.push 1 |> ignore
        let cons = ch.consumeAll() 
        cons |> fst |> should equal [1]
        cons |> snd |> fun c -> c.consumeAll() |> fst |> should equal []
        
    [<Test>]
    member this.range1() =
        let ch = Chain.empty()
        let r = Chain.range ch ch
        Seq.toList r |> should equal []

    [<Test>]
    member this.range2() =
        let ch = Chain.empty()
        let e = ch.push 1
        let r = Chain.range ch e
        Seq.toList r |> should equal [1]

    [<Test>]
    member this.rangeEmpty() =
        let ch = Chain.empty()
        let _ = ch.push 1
        let r =  Chain.range ch ch
        Seq.toList r |> should equal []

    [<Test>]
    member this.rangePartial() =
        let ch = Chain.empty()
        let e1 = ch.push 1
        let _ = e1.push 2
        let r = range ch e1
        Seq.toList r |> should equal [1]

    [<Test>]
    [<ExpectedException(typedefof<AlienChainError>)>]
    member this.rangeAlien() =
        let ch = Chain.empty()
        let ch2 = Chain.empty()
        let r = range ch ch2
        Seq.toList r |> ignore