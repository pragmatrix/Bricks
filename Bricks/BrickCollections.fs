module BrickCollections

open BrickDefs
open System.Collections.Immutable

module Set = 
    let empty<'e> = HashSet<'e>.Empty
    let ofSeq = HashSet.CreateRange

    type 'e change =
        | Added of 'e
        | Removed of 'e

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

    type 'e change = 
        | Inserted of int * 'e
        | Removed of int
