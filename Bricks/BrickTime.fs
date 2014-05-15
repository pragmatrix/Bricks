(** BRICKS IN TIME **)

module BrickTime

open Chain
open BrickDefs
open BrickCollections
open BricksCore
open BrickChannel

type b with
    static member track (source: 'v set brick) : 'v Set.changes channel =
        Channel.track Set.empty Set.diff source

    static member map (mapper: 's brick -> 't) (source: 's brick Set.changes channel) : 't Set.changes channel =

        let state = ref Map.empty<'s brick, 't>

        let processor (change : 's brick Set.change) = 
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

        fun (chain : 't Set.changes chain) changes ->
            changes |> Seq.flatten |> Seq.map processor |> chain.push

        |> Channel.makeProc source (Chain.empty())
                   
    static member materialize source =

        let state = ref Set.empty
        
        let processor change =
            let this = !state
            match change with
            | Set.Added e -> state := this.Add e
            | Set.Removed e -> state := this.Remove e

        fun _ changes ->
            changes |> Seq.flatten |> Seq.iter processor
            !state

        |> Channel.makeProc source Set.empty
