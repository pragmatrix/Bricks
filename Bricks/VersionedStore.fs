module VersionedStore

open System.Collections.Generic

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

type Store() = 
    let _store = Dictionary<obj, Versioned>()

    member this.store brick value = _store.Add(brick, value) 
    member this.remove brick = _store.Remove brick

         

    