(** BRICKS IN TIME **)

module BrickTime

open Chain
open BrickCollections
open BricksCore
open BrickChannel
open InlineHelper

open System.Collections.Immutable

let track (source: 'v iset brick) : 'v ISet.change channel =
    Channel.track ISet.reset ISet.diff source

let private mapList f channel = 

    let state = ref IList.empty

    let map (change, _) =
        let mChange =
            match change with
            | IList.Inserted (i, e) -> IList.Inserted(i, f e)
            | IList.Removed i -> IList.Removed i
            | IList.Reset v -> IList.map f v |> IList.Reset
        
        state := IList.apply mChange !state
        mChange, !state

    let reset (_, v) = map (IList.Reset v, v)

    Channel.map reset map channel

let private mapSet f channel = 

    let state = ref IMap.empty<'s, 't>
    let v' = ref ISet.empty<'t>

    let map (change, _) = 
        match change with
        | ISet.Added e ->
            let e' = f e
            state := (!state).Add(e, e')
            v' := (!v').Add(e')
            ISet.Added e', !v'

        | ISet.Removed e ->
            let state' = !state
            let e' = state'.[e]
            v' := (!v').Remove (state'.[e])
            state := state'.Remove e
            ISet.Removed e', !v'

        | ISet.Reset v ->
            state := v |> Seq.map (fun e -> (e, f e)) |> IMap.ofSeq
            v' := (!state).Values |> ISet.ofSeq
            let v'' = !v'
            ISet.Reset v'', v''

    let reset (_, v) = map (ISet.Reset v, v)

    Channel.map reset map channel 

let private materializeCollection (c : ('d * 'v) channel) = 
    brick {
        let! chain = c
        let (_, value) = chain.value
        return value
    }

type Materializer = Materializer with

    static member instance (Materializer, c: 'e ISet.change channel, _:'e iset brick) = fun () -> materializeCollection c
    static member instance (Materializer, c: 'e IList.change channel, _:'e ilist brick) = fun () -> materializeCollection c

let inline materialize source = Inline.instance(Materializer, source)()

type Mapper = Mapper with

    static member instance (Mapper, s: 'e ISet.change channel, _:'e2 ISet.change channel)  = fun f -> mapSet f s : 'e2 ISet.change channel
    static member instance (Mapper, l: 'e IList.change channel, _:'e2 IList.change channel) = fun f -> mapList f l : 'e2 IList.change channel
    static member instance (Mapper, l: 'e list, _:'e2 list) = fun f -> List.map f l : 'e2 list
    static member instance (Mapper, s: 'e seq, _:'e2 seq) = fun f -> Seq.map f s : 'e2 seq

let private map2List (f: 'a -> 'b -> 'r) (a : 'a ilist brick) (b : 'b ilist brick) = 
    brick {
        let! a = a
        let! b = b
        return Seq.map2 f a b |> IList.ofSeq
    }

type Mapper2 = Mapper2 with
    static member instance (Mapper2, a: 'a ilist brick, b: 'b ilist brick, _: 'r ilist brick) =
        fun (f: 'a -> 'b -> 'r) -> map2List f a b

let private foldList (f: 's -> 'e -> 's) (s: 's) (l : 'e ilist brick) : 's brick = 
    brick {
        let! l = l
        return Seq.fold f s l
    }

let private scanList (f: 's -> 'e -> 's) (s: 's) (l : 'e ilist brick) : 's ilist brick = 
    brick {
        let! l = l
        return Seq.scan f s l |> IList.ofSeq
    }

type Folder = Folder with
    static member instance (Folder, l: 'e ilist brick, f: 's -> 'e -> 's, _:'s brick) = 
        fun s -> foldList f s l

type Scanner = Scanner with
    static member instance (Scanner, l: 'e ilist brick, f: 's -> 'e -> 's, _:'s ilist brick) = 
        fun s -> scanList f s l
    static member instance (Scanner, l: 'e IList.change channel, f: 's -> 'e -> 's, _:'s ilist brick) =
        fun s -> l |> materialize |> scanList f s

module Published =
    let inline track source = track source
    let inline map f source = Inline.instance(Mapper, source) f
    let inline map2 f a b = Inline.instance(Mapper2, a, b) f
    let inline zip a b = Inline.instance(Mapper2, a, b) (fun a b -> (a,b))
    let inline materialize source = materialize source
    let inline fold f s source = Inline.instance(Folder, source, f) s
    let inline scan f s source = Inline.instance(Scanner, source, f) s

    [<assembly:AutoOpen("BrickTime.Published")>]
    do ()
