module BrickSetTests

open FsUnit
open NUnit.Framework

open Bricks

[<TestFixture>]
type BrickSetTests() =
    [<Test>]
    member this.testSetDiffAdd() = 
        let v1 = new obj()
 
        let s1 = set.ofSeq []
        let s2 = set.ofSeq [v1]
        let d = set.diff s1 s2
        d |> should equal [set.Added v1]

        let v2 = new obj()
        let s3 = set.ofSeq [v1; v2]
        let d = set.diff s2 s3
        d |> should equal [set.Added v2]

    [<Test>]
    member this.testSetDiffRemove() = 
        let v1 = new obj()
        let v2 = new obj()
 
        let s1 = set.ofSeq [v1; v2]
        let s2 = set.ofSeq [v2]
        let d = set.diff s1 s2
        d |> should equal [set.Removed v1]

        let s3 = set.ofSeq []
        let d = set.diff s2 s3
        d |> should equal [set.Removed v2]

    [<Test>]
    member this.setSetDiffAddRemove() =
        let v1 = new obj()
        let v2 = new obj()
        let v3 = new obj()

        let s1 = set.ofSeq [v1; v2]
        let s2 = set.ofSeq [v1; v3]
        let d = set.diff s1 s2
        d |> should equal [set.Removed v2; set.Added v3]

         
    [<Test>]
    member this.setNoDiff() =
        let v1 = new obj()
        let v2 = new obj()

        let s1 = set.ofSeq [v1; v2]
        let s2 = set.ofSeq [v1; v2]
        let d = set.diff s1 s2
        d |> should equal []
   
        

        