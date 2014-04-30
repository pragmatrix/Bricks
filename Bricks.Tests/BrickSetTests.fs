module BrickSetTests

open FsUnit
open NUnit.Framework

open Bricks

[<TestFixture>]
type BrickSetTests() =
    [<Test>]
    member this.testSetDiffAdd() = 
        let v1 = new obj()
 
        let s1 = Set.ofSeq []
        let s2 = Set.ofSeq [v1]
        let d = Set.diff s1 s2
        d |> should equal [Set.Added v1]

        let v2 = new obj()
        let s3 = Set.ofSeq [v1; v2]
        let d = Set.diff s2 s3
        d |> should equal [Set.Added v2]

    [<Test>]
    member this.testSetDiffRemove() = 
        let v1 = new obj()
        let v2 = new obj()
 
        let s1 = Set.ofSeq [v1; v2]
        let s2 = Set.ofSeq [v2]
        let d = Set.diff s1 s2
        d |> should equal [Set.Removed v1]

        let s3 = Set.ofSeq []
        let d = Set.diff s2 s3
        d |> should equal [Set.Removed v2]

    [<Test>]
    member this.setSetDiffAddRemove() =
        let v1 = new obj()
        let v2 = new obj()
        let v3 = new obj()

        let s1 = Set.ofSeq [v1; v2]
        let s2 = Set.ofSeq [v1; v3]
        let d = Set.diff s1 s2
        d |> should equal [Set.Removed v2; Set.Added v3]

         
    [<Test>]
    member this.setNoDiff() =
        let v1 = new obj()
        let v2 = new obj()

        let s1 = Set.ofSeq [v1; v2]
        let s2 = Set.ofSeq [v1; v2]
        let d = Set.diff s1 s2
        d |> should equal []
   
        

        