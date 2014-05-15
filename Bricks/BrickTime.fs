(** BRICKS IN TIME **)

module BrickTime

open Chain
open BrickDefs
open BrickCollections
open BricksCore
open BrickChannel

type b with
    // set

    static member track (source: 'v set brick) : 'v Set.change channel =
        Channel.track Set.empty Set.diff source

    static member map (mapper: 's -> 't) (source: 's Set.change channel) : 't Set.change channel =

        let state = ref Map.empty<'s, 't>

        let processor (change : 's Set.change) = 
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

        fun (chain : 't Set.change chain) changes ->
            changes |> Seq.map processor |> chain.pushSeq

        |> Channel.makeProc source (Chain.empty())
     
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

        |> Channel.makeProc source Set.empty

    // list

    (*

    static member map (mapper: 's -> 't) (source: 's List.change channel) : 't List.change channel =
        let processor (change : 's List.change) =
            match change with
            | List.Inserted (i, e) ->
                List.Inserted(i, mapper e)
            | List.Removed i ->
                List.Removed i

        fun (chain : 't List.change chain) change ->
            change |> processor |> chain.push

        |> Channel.makeProc source (Chain.empty())
    *)

