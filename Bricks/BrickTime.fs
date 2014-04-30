(** BRICKS IN TIME **)

module BrickTime

open Chain
open BrickDefs
open BrickSet
open BricksCore

(* Memo 

    A memo brick is a brick that is wrapped around another brick and remembers the previous value of it.

    The value of a memo brick is (previousValue, currentValue)

    tbd: kill memo, memo is unstable, because it does not support multiple consumers.
*)

let memo (def:'v) (source:'v brick) : ('v * 'v) brick =

    fun (b:('v*'v) brick) ->
        let value = source.evaluate()
        let prev = 
            match b.value with
            | Some (_, v) -> v
            | None -> def
        let newValue = (prev, value)
        [source :> Brick], newValue
    |> makeBrick

(*
    A diff brick is a memo brick and a diff function that is applied to output values of the memo.

    tbd: terminology is confusing: we need another name for diffs over time.
    tbd: diff is basically stable, because memo does not have more than one consumer
*)

let diff (def: 'v) (differ: 'v -> 'v -> 'd) (source: 'v brick) =
    let differ = uncurry differ
    source |> memo def |> convert differ

(* 
    Creates a brick that is able to track changes of a brick value.
*)

let inline private makeChannel f = makeBrickInit (Chain.empty()) f
type 'v channel = 'v chain brick

type ChannelProcessor<'s, 't> = 't -> 's seq -> 't

let inline private makeChannelProc (source : 's channel) (initial: 't) (f : ChannelProcessor<'s, 't>) = 
    let deps = [source :>Brick]
    let sourceHead = ref source.value.Value
    fun (this: 't brick) ->
        let sourceTail = source.evaluate()
        let values = Chain.range !sourceHead sourceTail
        sourceHead := sourceTail
        let chain = this.value.Value
        deps, f chain values
    |> makeBrickInit initial

let private _track (initial: 'v) (tracker: 'v -> 'v -> 'r) (source: 'v brick) : 'r channel =

    let current = ref initial

    fun (this: 'r chain brick) ->
        let chain = this.value.Value
        let value = source.evaluate()
        let res = tracker !current value
        current := value
        [source :> Brick], chain.push res
    |> makeChannel

type b with
    static member track (source: 'v set brick) : 'v Set.changes channel =
        _track Set.empty Set.diff source

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

        |> makeChannelProc source (Chain.empty())
                   
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

        |> makeChannelProc source Set.empty
