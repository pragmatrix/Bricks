module BrickDefs

open System.Collections.Immutable
open System.Collections.Generic

module List =
    let flatten l = List.collect id l

module Seq = 
    let flatten l = Seq.collect id l

type HashSet = ImmutableHashSet
type HashSet<'k> = ImmutableHashSet<'k>
type 'k set = HashSet<'k>

type HashMap = ImmutableDictionary
type HashMap<'k, 'v> = ImmutableDictionary<'k, 'v>

type ImmutableHashSet<'v> with
    member this.has v = this.Contains v

module Map =
    let empty<'k, 'v> = HashMap<'k, 'v>.Empty

    let get (m:HashMap<'k, 'v>) k = 
        let has, v = m.TryGetValue k
        if has then Some v else None
    let has (m:HashMap<'k, 'v>) k = m.ContainsKey k

type ImmutableDictionary<'k, 'v> with
    member this.get k = Map.get this k
    member this.has k = Map.has this k

type ImmutableDictionary with
    static member fromSeq seq = 
        ImmutableDictionary.CreateRange(Seq.map (fun (k, v) -> KeyValuePair(k, v)) seq)

let inline curry f a b = f (a, b)
let inline uncurry f (a, b) = f a b
let inline private (|?) (a: 'a option) b = if a.IsSome then a.Value else b

let inline isSame a b = obj.ReferenceEquals(a, b)

type b = class
    static member X = 0
    end
