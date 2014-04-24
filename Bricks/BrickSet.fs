module BrickSet

open BrickDefs

module bset = 
    let empty<'e> = HashSet<'e>.Empty
    let ofSeq = HashSet.CreateRange

    type Change<'v> =
        | Added of 'v
        | Removed of 'v

    let diff (s1: 'e set) (s2 : 'e set) = 
        // tbd: combine finding added / modified
        let added = 
            s2
            |> Seq.filter (s1.has >> not)
            |> Seq.map Added

        let removed = 
            s1 
            |> Seq.filter (s2.has >> not)
            |> Seq.map Removed

        [removed; added] |> Seq.flatten
