(** BRICKS IN TIME **)

module BrickTime

open Chain
open BrickDefs
open BrickCollections
open BricksCore
open BrickChannel

open System.Collections.Immutable


type b with

    // set

    static member track (source: 'v set brick) : 'v Set.change channel =
        Channel.track Set.empty Set.diff source

    static member mapc (mapper: 's -> 't) (source: 's channel) : 't channel = 
        fun (chain : 't chain) elements ->
            elements |> Seq.map mapper |> chain.pushSeq
        |> Channel.makeProcSeq source (Chain.empty())

    static member _map (mapper: 's -> 't, source: 's Set.change channel) : 't Set.change channel =

        let state = ref Map.empty<'s, 't>

        let mapper (change : 's Set.change) = 
            let this = !state
            match change with
            | Set.Added e -> 
                let t = mapper e
                state := this.Add(e, t)
                Set.Added t
            | Set.Removed e ->
                let t = this.[e]
                state := this.Remove e
                Set.Removed t

        b.mapc mapper source
     
    static member materialize source =

        let state = ref Set.empty
        
        let processor change =
            let this = !state
            match change with
            | Set.Added e -> state := this.Add e
            | Set.Removed e -> state := this.Remove e

        fun _ changes ->
            changes |> Seq.iter processor
            !state

        |> Channel.makeProcSeq source !state

    // list

    static member _map (mapper: 's -> 't, source: 's List.change channel) : 't List.change channel =
        let mapper (change : 's List.change) =
            match change with
            | List.Inserted (i, e) ->
                List.Inserted(i, mapper e)
            | List.Removed i ->
                List.Removed i

        b.mapc mapper source


    static member materialize source = 

        let state = ref ImmutableList.Empty

        let processor change = 
            let this = !state
            match change with
            | List.Inserted (i, e) -> state := this.Insert(i, e)
            | List.Removed i -> state := this.RemoveAt i

        fun _ changes ->
            changes |> Seq.iter processor
            !state

        |> Channel.makeProcSeq source !state


    static member inline map mapper source = b._map(mapper, source)
