﻿module Chain

open System.Collections
open System.Collections.Generic

(*
    A one producer / multiple consumer value chain
*)

type Chain = interface
    end

type Chain<'a> = { mutable next: ('a * Chain<'a>) option }
    with
        // the canonical IEnumerable is returning all values until the end of the chain
        interface IEnumerable<'a> 
            with 
                member this.GetEnumerator() : IEnumerator<'a> =
                    let gen = this.links() |> Seq.map fst
                    gen.GetEnumerator()
                member this.GetEnumerator() : IEnumerator =
                    (this :> IEnumerable<'a>).GetEnumerator() :> IEnumerator

        interface Chain

        member this.atEnd = this.next.IsNone

        // the value this chain is pointing to
        member this.value = 
            match this.next with
            | None -> failwith "chain.value: no value"
            | Some (v, _) -> v

        // Adds a value and returns the new end of the chain.
        member this.produce value =
            if (not this.atEnd) then
                failwith "chain.produce: not end of chain"
            let newEnd = Chain.empty()
            this.next <- Some (value, newEnd)
            newEnd

        // returns all the values and the end link
        member this.consumeAll() = 
            let a = this.links() |> Seq.toArray
            if a.Length = 0 then
                Seq.empty, this
            else
                a |> Array.unzip |> fst |> Seq.ofArray, a.[a.Length-1] |> snd

        static member empty() = { next = None }

        member this.append v = 
            match this.next with
            | None -> 
                this.produce v |> ignore
                this
            | Some (_, next) ->
                if (not next.atEnd) then failwith "chain.append: next must be end of chain"
                next.next <- Some (v, Chain.empty())
                next

        static member single v = 
            let c = Chain.empty()
            c.produce v |> ignore
            c

        // returns values alongside their next-links
        member private this.links() =
            this.next |> Seq.unfold (fun link -> 
                match link with
                | Some ((_, n) as l) -> Some (l, n.next)
                | None -> None
                )


type 'a chain = Chain<'a>
