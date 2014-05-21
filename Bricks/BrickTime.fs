(** BRICKS IN TIME **)

module BrickTime

open Chain
open BrickDefs
open BricksCore
open BrickChannel
open InlineHelper

open System.Collections.Immutable

let track (source: 'v iset brick) : 'v ISet.change channel =
    Channel.track ISet.empty ISet.diff source

let private mapc (mapper: 's -> 't) (source: 's channel) : 't channel = 
    fun (chain : 't chain) elements ->
        elements |> Seq.map mapper |> chain.pushSeq
    |> Channel.makeProcSeq source (Chain.empty())

let private mapList f l = 
    let cmapper (change : 's IList.change) =
        match change with
        | IList.Inserted (i, e) -> IList.Inserted(i, f e)
        | IList.Removed i -> IList.Removed i

    mapc cmapper l

let private materializeSet collection = 
    let state = ref ISet.empty
        
    let processor change =
        let this = !state
        match change with
        | ISet.Added e -> state := this.Add e
        | ISet.Removed e -> state := this.Remove e

    fun _ changes ->
        changes |> Seq.iter processor
        !state

    |> Channel.makeProcSeq collection !state

let private mapSet f s = 
    let state = ref IMap.empty<'s, 't>

    let cmapper (change : 's ISet.change) = 
        let this = !state
        match change with
        | ISet.Added e -> 
            let t = f e
            state := this.Add(e, t)
            ISet.Added t
        | ISet.Removed e ->
            let t = this.[e]
            state := this.Remove e
            ISet.Removed t

    mapc cmapper s

let private materializeList c = 
        let state = ref IList.empty

        let processor change = 
            let this = !state
            match change with
            | IList.Inserted (i, e) -> state := this.Insert(i, e)
            | IList.Removed i -> state := this.RemoveAt i

        fun _ changes ->
            changes |> Seq.iter processor
            !state

        |> Channel.makeProcSeq c !state



type Materializer = Materializer with

    static member instance (Materializer, c: 'e ISet.change channel, _:ImmutableHashSet<'e> brick) = fun () -> materializeSet c
    static member instance (Materializer, c: 'e IList.change channel, _:'e ilist brick) = fun () -> materializeList c

let inline materialize source = Inline.instance(Materializer, source)()

type Mapper = Mapper with

    static member instance (Mapper, s: 'e ISet.change channel, _:'e2 ISet.change channel)  = fun f -> mapSet f s : 'e2 ISet.change channel
    static member instance (Mapper, l: 'e IList.change channel, _:'e2 IList.change channel) = fun f -> mapList f l : 'e2 IList.change channel
    static member instance (Mapper, l: 'e list, _:'e2 list) = fun f -> List.map f l : 'e2 list
    static member instance (Mapper, s: 'e seq, _:'e2 seq) = fun f -> Seq.map f s : 'e2 seq

let private mapList2 (f: 'a -> 'b -> 'r) (a : 'a ilist brick) (b : 'b ilist brick) = 
    brick {
        let! a = a
        let! b = b
        return Seq.map2 f a b |> IList.ofSeq
    }

type Mapper2 = Mapper2 with
    static member instance (Mapper2, a: 'a ilist brick, b: 'b ilist brick, _: 'r ilist brick) =
        fun f -> mapList2 f a b

let private foldList (f: 's -> 'e -> 's) (s: 's) (l : 'e ilist brick) : 's brick = 
    brick {
        let! l = l
        return Seq.fold f s l
    }

let private scanList (f: 's -> 'e -> 's) (s: 's) (l : 'e ilist  brick) : 's ilist brick = 
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
    let inline materialize source = materialize source
    let inline fold f s source = Inline.instance(Folder, source, f) s
    let inline scan f s source = Inline.instance(Scanner, source, f) s

    [<assembly:AutoOpen("BrickTime.Published")>]
    do ()
