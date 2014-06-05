module Chain

open System.Collections
open System.Collections.Generic


(*
    A one producer / multiple consumer value chain
*)

let private isSame a b = obj.ReferenceEquals(a, b)

type Chain = interface
    end

type Chain<'e> = { value: 'e; mutable next: Chain<'e> option }
    with
        // the canonical IEnumerable is returning all values until the end of the chain
        interface IEnumerable<'e> 
            with 
                member this.GetEnumerator() : IEnumerator<'e> =
                    let e = this.all() :> IEnumerable<'e> 
                    e.GetEnumerator()
                member this.GetEnumerator() : IEnumerator =
                    (this :> IEnumerable<'e>).GetEnumerator() :> IEnumerator

        interface Chain

        member this.atEnd = this.next.IsNone
            
        // the back of this chain or atEnd
        member this.back = 
            match this.next with
            | None -> this
            | Some next -> next.back

        // Adds a value and returns the new end of the chain.
        member this.push value =
            if (not this.atEnd) then
                failwith "chain.produce: not back of chain"
            let next = { value = value; next = None }
            this.next <- Some next
            next             
                     
        member this.pushSeq values = 
            values |> (Seq.fold (fun (c : Chain<'e>) v -> c.push v)) this

        member this.append v = this.push v |> ignore

        static member single v = 
            { value = v; next = None }

        static member ofSeq (s: 'e seq) =
            let h = Chain.single (Seq.head s)
            h.pushSeq (Seq.skip 1 s) |> ignore
            h

        member this.all() =
            match this.next with
            | None -> [this.value]
            | Some next -> this.value :: next.all()

type 'a chain = Chain<'a>

exception AlienChainError of string

let range (b : 'e chain) (e : 'e chain) = 
    let c = ref b
    seq {
        yield (!c).value
        while (not ((!c).atEnd || isSame !c e)) do
            c := (!c).next.Value
            yield (!c).value

        if (not (isSame !c e)) then
            raise (AlienChainError "end of chain is from a different chain")
    }
