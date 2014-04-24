module BrickSetTests

open FsUnit
open NUnit.Framework

open Bricks

[<TestFixture>]
type BrickSetTests() =
    [<Test>]
    member this.testSetDiffAdd() = 
        let v1 = new obj()
 
        let s1 = bset.ofSeq []
        let s2 = bset.ofSeq [v1]
        let d = bset.diff s1 s2
        d |> should equal [bset.Added v1]

        let v2 = new obj()
        let s3 = bset.ofSeq [v1; v2]
        let d = bset.diff s2 s3
        d |> should equal [bset.Added v2]

    [<Test>]
    member this.testSetDiffRemove() = 
        let v1 = new obj()
        let v2 = new obj()
 
        let s1 = bset.ofSeq [v1; v2]
        let s2 = bset.ofSeq [v2]
        let d = bset.diff s1 s2
        d |> should equal [bset.Removed v1]

        let s3 = bset.ofSeq []
        let d = bset.diff s2 s3
        d |> should equal [bset.Removed v2]

    [<Test>]
    member this.setSetDiffAddRemove() =
        let v1 = new obj()
        let v2 = new obj()
        let v3 = new obj()

        let s1 = bset.ofSeq [v1; v2]
        let s2 = bset.ofSeq [v1; v3]
        let d = bset.diff s1 s2
        d |> should equal [bset.Removed v2; bset.Added v3]

         
    [<Test>]
    member this.setNoDiff() =
        let v1 = new obj()
        let v2 = new obj()

        let s1 = bset.ofSeq [v1; v2]
        let s2 = bset.ofSeq [v1; v2]
        let d = bset.diff s1 s2
        d |> should equal []
   
        

        