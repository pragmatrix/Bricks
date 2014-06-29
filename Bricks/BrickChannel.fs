module BrickChannel

open BricksCore

type 'v channel = 'v brick

module Channel =
    type Processor<'s, 't> = 's seq -> 't seq

    let mapChunked (reset: 's -> 't) (f : Processor<'s, 't>) (source : 's channel) : 't channel= 

        brick {
            let! current = historyOf source
            match current with
            | Reset v -> yield reset v
            | Progress values -> yield! Progress (f values)
        }

    let inline source v = lift v

    // push an element into a channel
    
    // tbd: what if the channel was never evaluated in between, may be we can only change the
    // channel's evaluation function, but not its value.
    
    (*

    let push (c: 'e channel) (element: 'e) =
        match c.value with
        | None -> c.write (Chain.single(element))
        | Some v -> c.write (v.push element)

    *)

    // Create a channel by applying a diff function to a brick
    let track (reset: 'v -> 'r) (diff: 'v -> 'v -> 'r seq) (source: 'v brick) : 'r channel =

        brick {
            let! prev = previousOf source
            let! s = source
            match prev with
            | None ->
                return (reset s)
            | Some prev ->
                let d = diff prev s
                return! Progress d 
        }

    let map (reset: 's -> 't) (mapper: 's -> 't) (source: 's channel) : 't channel = 
        mapChunked reset (Seq.map mapper) source

[<assembly:AutoOpen("BrickChannel")>]
do ()
