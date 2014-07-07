module BricksProgram

open System

open Collections
open BricksCore

let private managedBeaconsAndDestructors node =
    let tag = obj()
 
    let rec aux soFar todo =
        match todo with
        | [] -> List.rev soFar
        | []::rest -> aux soFar rest
        | ((b, node) :: rest1) :: rest2 ->
            let rest = rest1 :: rest2
            if b.tag = tag then 
                aux soFar rest
            else
                b.tag <- tag
                match node with
                | Mutable -> aux soFar rest
                | Computed deps -> aux soFar (deps::rest)
                | Managed (dep, destructor) ->
                    aux ((b, destructor)::soFar) ([dep]::rest)

    aux [] [[node]]

(* PROGRAM *)

type private Counter = int64
type private Destructor = unit -> unit

type LiveMap = IMap<Beacon, int64 * Destructor>

type 'v program(root : 'v brick) =

    let mutable _managedTick = 0L
    let mutable _liveSet : Beacon iset = ISet.empty
    let mutable _liveMap : LiveMap = IMap.empty
            
    interface IDisposable with
        member this.Dispose() = ()

    member this.run() = 
        let result = root.evaluate()
        let rootNode = root.node
        let live = managedBeaconsAndDestructors rootNode
        this.updateLive live
        result
       
        
    member this.updateLive active =
        let activeSet = active |> Seq.map fst |> ISet.ofSeq
        let activeDestructors = active |> IMap.ofSeq

        let removed, added = ISet.diff _liveSet activeSet
        let removed = removed |> Seq.toList
        let added = added |> Seq.toList

        let removeInactive (liveMap:LiveMap) =
            removed 
                |> Seq.map (fun beacon -> liveMap.[beacon])
                |> Seq.sortBy fst
                |> Seq.toList
                |> List.rev
                |> List.iter (fun (_, destructor) -> destructor())
            removed |> Seq.fold (fun (map:LiveMap) beacon -> map.Remove beacon) liveMap

        let addActive managedTick liveMap = 

            let nextnextTick = managedTick + int64(added.Length)
            let newTicks = 
                if added.Length = 0 then []
                else [managedTick .. nextnextTick-1L]

            let addedWithTick = 
                List.zip added newTicks
                |> List.map (fun (b, t) -> (b, (t, activeDestructors.[b])) )
            let newLiveMap = addedWithTick |> Seq.fold (fun (map:LiveMap) v -> map.Add(v)) liveMap
            nextnextTick, newLiveMap


        _liveMap <- removeInactive _liveMap
        let newTick, newLiveMap = addActive _managedTick _liveMap
        _managedTick <- newTick
        _liveMap <- newLiveMap

        _liveSet <- activeSet
        assert (_liveSet.Count = _liveMap.Count)


        



        
        
        
        
    member this.apply(t: Transaction) = t()

let toProgram b = new (_ program)(b)

[<assembly:AutoOpen("BricksProgram")>] ()
