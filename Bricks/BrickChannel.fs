module BrickChannel

open BrickDefs
open BricksCore

open Chain

type 'v channel = 'v chain brick

module Channel =
    let make f = makeBrickInit (Chain.empty()) f

    type Processor<'s, 't> = 't -> 's seq -> 't

    let makeProc (source : 's channel) (initial: 't) (f : Processor<'s, 't>) = 
        let deps = [source :>Brick]
        let sourceHead = ref source.value.Value
        fun (this: 't brick) ->
            let sourceTail = source.evaluate()
            let values = Chain.range !sourceHead sourceTail
            sourceHead := sourceTail
            let chain = this.value.Value
            deps, f chain values
        |> makeBrickInit initial

    let source<'e>() =
        fun (b : 'e channel) -> [], b.value.Value
        |> make

    // push an element into a source channel
    let push (c: 'e channel) (element: 'e) =
        let v = c.value.Value
        c.write (v.push element)

    // Create a channel by applying a diff function to a brick
    let track (initial: 'v) (tracker: 'v -> 'v -> 'r) (source: 'v brick) : 'r channel =

        let current = ref initial

        fun (this: 'r chain brick) ->
            let chain = this.value.Value
            let value = source.evaluate()
            let res = tracker !current value
            current := value
            [source :> Brick], chain.push res
        |> make