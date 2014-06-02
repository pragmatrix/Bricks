module BrickSetTests

open FsUnit
open NUnit.Framework

open Bricks

[<TestFixture>]
type BrickSetTests() =
    [<Test>]
    member this.testSetDiffAdd() = 
        let v1 = new obj()
 
        let s1 = ISet.ofSeq []
        let s2 = ISet.ofSeq [v1]
        let d = ISet.diff s1 s2
        d |> Seq.map fst |> should equal [ISet.Added v1]

        let v2 = new obj()
        let s3 = ISet.ofSeq [v1; v2]
        let d = ISet.diff s2 s3
        d |> Seq.map fst |> should equal [ISet.Added v2]

    [<Test>]
    member this.testSetDiffRemove() = 
        let v1 = new obj()
        let v2 = new obj()
 
        let s1 = ISet.ofSeq [v1; v2]
        let s2 = ISet.ofSeq [v2]
        let d = ISet.diff s1 s2
        d |> Seq.map fst |> should equal [ISet.Removed v1]

        let s3 = ISet.ofSeq []
        let d = ISet.diff s2 s3
        d |> Seq.map fst |> should equal [ISet.Removed v2]

    [<Test>]
    member this.setSetDiffAddRemove() =
        let v1 = new obj()
        let v2 = new obj()
        let v3 = new obj()

        let s1 = ISet.ofSeq [v1; v2]
        let s2 = ISet.ofSeq [v1; v3]
        let d = ISet.diff s1 s2
        d |> Seq.map fst |> should equal [ISet.Removed v2; ISet.Added v3]

         
    [<Test>]
    member this.setNoDiff() =
        let v1 = new obj()
        let v2 = new obj()

        let s1 = ISet.ofSeq [v1; v2]
        let s2 = ISet.ofSeq [v1; v2]
        let d = ISet.diff s1 s2
        d |> should equal []
   
        

        