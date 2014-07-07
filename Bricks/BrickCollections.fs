(** BRICKS IN TIME **)

module BrickCollections

open Collections
open BricksCore

open System.Collections.Immutable

[<assembly:AutoOpen("BrickCollections")>] ()

module BSet =

    let track (source: 'v iset brick) : 'v ISet.change brick =

        let folder (previous, _) current = current, (ISet.ediff previous current)

        Value.foldp folder (ISet.empty, []) source
        |> Signal.map snd
        |> Signal.flatten

    let entangle (initial: 'v iset) (source: 'v ISet.event brick) =
        let folder (_, set: 'v iset) change =
            change, ISet.apply change set

        Signal.foldp folder (ISet.Reset initial, initial) source

    let map (f : 's -> 't) (source : 's ISet.change brick) : 't ISet.change brick = 

        let folder (state : (IMap<'s, 't> * ISet.event<'t>) option) (change, materialized) =

            let resetMap set = 
                set |> Seq.map (fun e -> (e, f e)) |> IMap.ofSeq

            Some <|
                match state with 
                | None -> 
                    let map = resetMap materialized
                    map, ISet.Reset (map.Values |> ISet.ofSeq)

                | Some (map, _) ->
                    match change with
                    | ISet.Added s ->
                        let t = f s
                        let map = map.Add(s, t)
                        map, ISet.Added t

                    | ISet.Removed e ->
                        let t = map.[e]
                        let map = map.Remove(e)
                        map, ISet.Removed t

                    | ISet.Reset set ->
                        let map = resetMap set
                        map, ISet.Reset (map.Values |> ISet.ofSeq)

        source
        |> Signal.foldp folder None
        |> Signal.map (fun x -> x.Value |> snd)
        |> entangle ISet.empty

    let materialize (source: 's ISet.change brick) = 
        Value.map snd source

module BList = 

    let entangle (initial: 'v ilist) (source : 'v IList.event brick) =
        let folder (_, list: 'v ilist) change = 
            change, IList.apply change list

        Signal.foldp folder (IList.Reset initial, initial) source

    let map (f: 's -> 't) (source : 's IList.change brick) : 't IList.change brick =

        let folder (state : 't IList.change option) (change, materialized) =

            let resetList list = 
                list |> IList.map f

            Some <|
                match state with
                | None ->
                    let list = resetList materialized
                    IList.Reset list, list

                | Some (_, list) ->
                    let event = 
                        match change with
                        | IList.Inserted (i, e) -> IList.Inserted(i, f e)
                        | IList.Removed i -> IList.Removed i
                        | IList.Reset l -> resetList l |> IList.Reset
                    let list = IList.apply event list
                    event, list


        source 
        |> Signal.foldp folder None
        |> Signal.map Option.get
        
    let materialize (source: 's IList.change brick) =
        Value.map snd source
