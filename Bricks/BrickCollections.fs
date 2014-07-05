(** BRICKS IN TIME **)

module BrickCollections

open Collections
open BricksCore
open BrickChannel

open System.Collections.Immutable

[<assembly:AutoOpen("BrickCollections")>] ()

module BSet =

    let track (source: 'v iset brick) : 'v ISet.change brick =

        let folder (previous, _) current = current, (ISet.diff previous current)

        Value.foldp folder (ISet.empty, []) source
        |> Signal.map snd
        |> Signal.flatten

    let entangle (initial: 'v iset) (source: 'v ISet.event brick) =
        let folder (_, set: 'v iset) change =
            change, ISet.apply change set

        Signal.foldp folder (ISet.Reset initial, initial) source

    let map (f : 's -> 't) (source : 's ISet.change brick) : 't ISet.change brick = 

        let folder (state : (IMap<'s, 't> * ISet.event<'t>) option) (change, materialized) =

            let resetMap set = 
                set |> Seq.map (fun e -> (e, f e)) |> IMap.ofSeq

            Some <|
                match state with 
                | None -> 
                    let map = resetMap materialized
                    map, ISet.Reset (map.Values |> ISet.ofSeq)

                | Some (map, _) ->
                    match change with
                    | ISet.Added s ->
                        let t = f s
                        let map = map.Add(s, t)
                        map, ISet.Added t

                    | ISet.Removed e ->
                        let t = map.[e]
                        let map = map.Remove(e)
                        map, ISet.Removed t

                    | ISet.Reset set ->
                        let map = resetMap set
                        map, ISet.Reset (map.Values |> ISet.ofSeq)

        source
        |> Signal.foldp folder None
        |> Signal.map (fun x -> x.Value |> snd)
        |> entangle ISet.empty

    let materialize (source: 's ISet.change brick) = 
        Value.map snd source

module BList = 

    let entangle (initial: 'v ilist) (source : 'v IList.event brick) =
        let folder (_, list: 'v ilist) change = 
            change, IList.apply change list

        Signal.foldp folder (IList.Reset initial, initial) source

    let map (f: 's -> 't) (source : 's IList.change brick) : 't IList.change brick =

        let folder (state : 't IList.change option) (change, materialized) =

            let resetList list = 
                list |> IList.map f

            Some <|
                match state with
                | None ->
                    let list = resetList materialized
                    IList.Reset list, list

                | Some (_, list) ->
                    let event = 
                        match change with
                        | IList.Inserted (i, e) -> IList.Inserted(i, f e)
                        | IList.Removed i -> IList.Removed i
                        | IList.Reset l -> resetList l |> IList.Reset
                    let list = IList.apply event list
                    event, list


        source 
        |> Signal.foldp folder None
        |> Signal.map Option.get
        
    let materialize (source: 's IList.change brick) =
        Value.map snd source

(*

let private materializeCollection (s : ('d * 'v) channel) = lift snd s

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

    [<assembly:AutoOpen("BrickTime.Published")>] ()
*)
