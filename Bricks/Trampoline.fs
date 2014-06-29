﻿module Trampoline

// A monadic trampoline, used to wrap computations so that they don't consume stack space.
//
// from http://www.fssnip.net/dK
// with thanks to Nick Palladinos

type TrampValue<'T> =
    | DelayValue of Delay<'T>
    | ReturnValue of Return<'T>
    | BindValue of IBind<'T>

and ITramp<'T> = 
    abstract member Value : TrampValue<'T>
    abstract member Run : unit -> 'T

and Delay<'T>(f : unit -> ITramp<'T>) =
    member self.Func = f 
    interface ITramp<'T> with
        member self.Value = DelayValue self
        member self.Run () = (f ()).Run()

and Return<'T>(x :'T) = 
    member self.Value = x
    interface ITramp<'T> with
        member self.Value = ReturnValue self
        member self.Run () = x

and IBind<'T> = 
    abstract Bind<'R> : ('T -> ITramp<'R>) -> ITramp<'R>

and Bind<'T, 'R>(tramp : ITramp<'T>, f : ('T -> ITramp<'R>)) = 
    interface IBind<'R> with
        member self.Bind<'K>(f' : 'R -> ITramp<'K>) : ITramp<'K> =
            new Bind<'T, 'K>(tramp, fun t -> new Bind<'R, 'K>(f t, (fun r -> f' r)) :> _) :> _
    interface ITramp<'R> with
        member self.Value = BindValue self
        member self.Run () = 
            match tramp.Value with
            | BindValue b -> b.Bind(f).Run() 
            | ReturnValue r -> (f r.Value).Run()
            | DelayValue d -> (new Bind<'T, 'R>(d.Func (), f) :> ITramp<'R>).Run() 

// Builder
type TrampBuilder() = 
    member self.Return a = new Return<_>(a) :> ITramp<_>
    member self.Bind(tramp, f) = 
        new Bind<'T, 'R>(tramp, f) :> ITramp<'R>
    member self.Delay f = 
        new Delay<_>(f) :> ITramp<_>
   
let tramp = new TrampBuilder()

let trampSeq (s: seq<ITramp<'v>>) : ITramp<'v list> =
    let rec ts todo =
        tramp {
            match todo with
            | head::rest ->
                let! v = head
                let! r = ts rest
                return v::r
            | [] ->
                return []
        }

    s |> Seq.toList |> ts

let map f t = 
    tramp {
        let! v = t
        return f v
    }
