module BrickCursor

open BrickCollections
open BrickChannel
open BricksCore
open BrickTime


type ListCursor<'e> = { source: 'e IList.change channel; index: int brick }

type 'e lcursor = ListCursor<'e>

let cursor source index = 
    
    let state = ref index

    let processor change =
        let index = !state
        match change with
        | IList.Inserted(i, _) -> if i <= index then state := index + 1
        | IList.Removed i -> if i < index then state := index - 1
        | IList.Reset _ -> state := 0

    fun _ changes ->
        changes |> Seq.iter processor
        !state

    |> Channel.makeProcSeq source !state 
    |> fun b -> (source, b)

    