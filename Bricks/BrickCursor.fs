module BrickCursor

open BrickCollections
open BrickChannel
open BricksCore
open BrickTime


type cursor = int brick

let cursor (source : IList.change<'e> brick) : cursor = 
    
    let processor index (change, _) =
        match change with
        | IList.Inserted(i, _) -> if i <= index then index + 1 else index
        | IList.Removed i -> if i < index then index - 1 else index
        | IList.Reset _ -> 0

    brick {
        let! s = valueOfSelf
        let! h = historyOf source

        match h with
        | Reset _ -> 
            return 0
        | Progress changes -> 
            let index = s.Value
            let index = Seq.fold processor index changes
            return index
    }


    