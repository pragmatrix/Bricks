module BrickChannel

open BricksCore

open Chain

type 'v channel = 'v chain brick

module Channel =
    type Processor<'s, 't> = 't -> 's seq -> 't

    let makeProcSeq (reset: 's -> 't) (f : Processor<'s, 't>) (source : 's channel) = 

        let prevHead = ref None

        brick {
            let! current = source
            let! self = selfValue
            match !prevHead with
            | None ->
                let t = reset current.value
                prevHead := Some current
                return t
            | Some previous ->
                let sourceValues = Chain.range previous current |> Seq.skip 1
                prevHead := Some current
                return (f self.Value sourceValues)
        }

    let source v = Chain.single v |> value

    // push an element into a channel
    
    // tbd: what if the channel was never evaluated in between, may be we can only change the
    // channel's evaluation function, but not its value.
    
    let push (c: 'e channel) (element: 'e) =
        match c.value with
        | None -> c.write (Chain.single(element))
        | Some v -> c.write (v.push element)

    // Create a channel by applying a diff function to a brick
    let track (reset: 'v -> 'r) (diff: 'v -> 'v -> 'r seq) (source: 'v brick) : 'r channel =

        let prevSet = ref None

        brick {
            let! s = source
            let! (sv: 'r chain option) = selfValue
            let res = 
                match sv with
                | None -> 
                    Chain.single(reset s)
                | Some rc -> 
                    let diffs = diff (!prevSet).Value s
                    rc.pushSeq diffs
            prevSet := Some s
            return res
        }

    let map (reset: 's -> 't) (mapper: 's -> 't) (source: 's channel) : 't channel = 

        let reset s = 
            Chain.single (reset s)

        let processor (chain : 't chain) elements =
            elements |> Seq.map mapper |> chain.pushSeq
        
        makeProcSeq reset processor source

[<assembly:AutoOpen("BrickChannel")>]
do ()
