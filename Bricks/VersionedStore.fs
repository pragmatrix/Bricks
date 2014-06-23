module VersionedStore

open System.Collections.Generic

open BrickCollections

type Versioned = interface
    end

type Versioned<'v> = { value: 'v; mutable next: Versioned<'v> option } with

    interface Versioned

    static member single (value: 'v) = { value = value; next = None }

    static member ofSeq (s: 'v seq) =
        let h = Versioned.single (Seq.head s)
        h.pushSeq (Seq.skip 1 s) |> ignore
        h

    member this.push (value: 'v) =
        assert(this.next.IsNone)
        let n = Versioned<'v>.single value
        this.next <- Some n
        n

    member this.head = this.value

    member this.tail =
        this.next
        |> Seq.unfold (Option.map (fun n -> n.value, n.next))
        |> Seq.toList

    member this.pushSeq values = 
        values |> Seq.fold (fun (c : Versioned<_>) v -> c.push v) this


type Brick =
    abstract member addReferrer : Brick -> unit
    abstract member removeReferrer : Brick -> unit
    abstract member tryCollect : Store -> unit
    abstract member invalidate : unit -> unit

and StoreRecord = { value: Versioned; trace: IMap<Brick, Versioned> }

and Store() = 
    let _store = Dictionary<obj, StoreRecord>()

    member this.store brick record = _store.[brick] <- record
    member this.remove brick = _store.Remove brick
    member this.tryGet brick = _store.TryGetValue brick
         

type Store with
    member this.get brick = 
        this.tryGet brick |> snd

    member this.tryGetValue brick = 
        match this.tryGet brick with
        | (true, { value = value }) -> Some value
        | _ -> None

    member this.getValue brick = 
        (this.tryGetValue brick).Value

    member this.getTraceOrDefault brick =
        match this.tryGet brick with
        | (true, { trace = trace }) -> trace
        | _ -> IMap.empty
