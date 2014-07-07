module Collections

open System.Collections.Immutable
open System.Collections.Generic
open System.Collections.Immutable

[<assembly:AutoOpen("Collections")>] ()

module List =
    let flatten l = List.collect id l

module Seq = 
    let flatten l = Seq.collect id l

// The collection from System.Collections.Immutable are used to get rid
// of the F# comparison constrained and do comparisons based on references if not
// overridden by Equals() ... well, and I also suspect that they are faster. 

module ISet = 
    type ImmutableHashSet<'v> with
        member this.has v = this.Contains v

    type 'e iset = ImmutableHashSet<'e>
    let empty<'e> = ImmutableHashSet<'e>.Empty
    let ofSeq = ImmutableHashSet.CreateRange

    type 'e event =
        | Added of 'e
        | Removed of 'e
        | Reset of 'e iset

    type 'e change = 'e event * 'e iset

    let apply change (value : 'e iset) = 
        match change with
        | Added e -> value.Add e
        | Removed e -> value.Remove e
        | Reset v -> v

    let materialize initial changes = 
        let apply (_, v) c = c, apply c v
        Seq.scan apply (Reset initial, initial) changes |> Seq.skip 1

    let diff (s1: 'e iset) (s2: 'e iset) =
        let added = 
            s2
            |> Seq.filter (s1.has >> not)

        let removed = 
            s1 
            |> Seq.filter (s2.has >> not)

        removed, added

    let ediff (s1: 'e iset) (s2 : 'e iset) =
        let removed, added = diff s1 s2
        let removed = removed |> Seq.map Removed
        let added = added |> Seq.map Added

        [removed; added] |> Seq.flatten |> materialize s1 |> Seq.toList
        
    let reset (s: 'e iset) = (Reset s, s)

    let inline map f = Seq.map f >> ofSeq

type 'e iset = 'e ISet.iset

module IList =
    type 'e ilist = ImmutableList<'e>

    let empty<'e> = ImmutableList<'e>.Empty
    let ofSeq = ImmutableList.CreateRange

    type 'e event = 
        | Inserted of int * 'e
        | Removed of int
        | Reset of 'e ilist

    type 'e change = 'e event * 'e ilist

    let apply change (value : 'e ilist) = 
        match change with
        | Inserted (i, e) -> value.Insert(i, e)
        | Removed i -> value.RemoveAt(i)
        | Reset v -> v

    let materialize initial changes = 
        let apply (_, v) c = c, apply c v
        Seq.scan apply (Reset initial, initial) changes |> Seq.skip 1

    let inline reset (l: 'e ilist) = (Reset l, l)

    let inline map f = Seq.map f >> ofSeq

type 'e ilist = 'e IList.ilist

module IMap =
    type IMap<'k, 'v> = ImmutableDictionary<'k, 'v>
    let empty<'k, 'v> = IMap<'k, 'v>.Empty

    let get (m:IMap<'k, 'v>) k = 
        let has, v = m.TryGetValue k
        if has then Some v else None

    let has (m:IMap<'k, 'v>) k = m.ContainsKey k

    let ofSeq s = ImmutableDictionary.CreateRange(Seq.map (fun (k, v) -> KeyValuePair(k, v)) s)

type IMap<'k, 'v> = IMap.IMap<'k, 'v>

type ImmutableDictionary<'k, 'v> with
    member this.get k = IMap.get this k
    member this.has k = IMap.has this k

type ImmutableDictionary with
    static member ofSeq s = IMap.ofSeq s

let inline curry f a b = f (a, b)
let inline uncurry f (a, b) = f a b
let inline private (|?) (a: 'a option) b = if a.IsSome then a.Value else b

let inline isSame a b = obj.ReferenceEquals(a, b)

