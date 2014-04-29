module BrickSet

open BrickDefs

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
