module BrickCollections

open BrickDefs
open System.Collections.Immutable

module Set = 
    let empty<'e> = HashSet<'e>.Empty
    let ofSeq = HashSet.CreateRange

    type 'v change =
        | Added of 'v
        | Removed of 'v

    type 'v changes = 'v change seq

    let diff (s1: 'e set) (s2 : 'e set) = 
        let added = 
            s2
            |> Seq.filter (s1.has >> not)
            |> Seq.map Added

        let removed = 
            s1 
            |> Seq.filter (s2.has >> not)
            |> Seq.map Removed

        [removed; added] |> Seq.flatten

module List =
    type 'e t = ImmutableList<'e>

    let empty<'e> = ImmutableList<'e>.Empty
    let ofSeq = ImmutableList.CreateRange

    type 'v change = 
        | Inserted of int * 'v
        | Removed of int
