module BrickDefs

open System.Collections.Immutable
open System.Collections.Generic
open System.Collections.Immutable

module List =
    let flatten l = List.collect id l

module Seq = 
    let flatten l = Seq.collect id l

// The collection from System.Collections.Immutable are used to get rid
// of the F# comparison constrained and do comparison based on references if not
// overridden by Equals()

module ISet = 
    type ImmutableHashSet<'v> with
        member this.has v = this.Contains v

    type 'e iset = ImmutableHashSet<'e>
    let empty<'e> = ImmutableHashSet<'e>.Empty
    let ofSeq = ImmutableHashSet.CreateRange

    type 'e change =
        | Added of 'e
        | Removed of 'e

    let diff (s1: 'e iset) (s2 : 'e iset) = 
        let added = 
            s2
            |> Seq.filter (s1.has >> not)
            |> Seq.map Added

        let removed = 
            s1 
            |> Seq.filter (s2.has >> not)
            |> Seq.map Removed

        [removed; added] |> Seq.flatten

type 'e iset = 'e ISet.iset

module IList =
    type 'e ilist = ImmutableList<'e>

    let empty<'e> = ImmutableList<'e>.Empty
    let ofSeq = ImmutableList.CreateRange

    type 'e change = 
        | Inserted of int * 'e
        | Removed of int

type 'e ilist = 'e IList.ilist

module IMap =
    type IMap<'k, 'v> = ImmutableDictionary<'k, 'v>
    let empty<'k, 'v> = IMap<'k, 'v>.Empty

    let get (m:IMap<'k, 'v>) k = 
        let has, v = m.TryGetValue k
        if has then Some v else None
    let has (m:IMap<'k, 'v>) k = m.ContainsKey k

type IMap<'k, 'v> = IMap.IMap<'k, 'v>

type ImmutableDictionary<'k, 'v> with
    member this.get k = IMap.get this k
    member this.has k = IMap.has this k

type ImmutableDictionary with
    static member fromSeq seq = 
        ImmutableDictionary.CreateRange(Seq.map (fun (k, v) -> KeyValuePair(k, v)) seq)

let inline curry f a b = f (a, b)
let inline uncurry f (a, b) = f a b
let inline private (|?) (a: 'a option) b = if a.IsSome then a.Value else b

let inline isSame a b = obj.ReferenceEquals(a, b)

[<assembly:AutoOpen("BrickDefs")>]
do ()

