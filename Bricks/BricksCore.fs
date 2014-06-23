module BricksCore

(** BRICKS, F# COMPUTATION EXPRESSIONS AND COMBINATORS FOR LAZY INCREMENTAL COMPUTATION, SEE README.MD **)

open System
open System.Diagnostics
open System.Collections.Immutable
open System.Collections.Generic
open System.Linq

open BrickCollections
open VersionedStore

open InlineHelper
open Trampoline

(** BRICK, ENVIRONMENT **)

type Brick =
    abstract member addReferrer : Brick -> unit
    abstract member removeReferrer : Brick -> unit
    abstract member tryCollect : Store -> unit
    abstract member invalidate : unit -> unit
    abstract member versioned : Store -> Versioned

[<AutoOpen>]
module BrickExtensions =
    type Brick with
        member this.removeReferrerAndTryCollect referrer store =
            this.removeReferrer referrer
            this.tryCollect store




type internal Trace = Brick list

type History<'v> = 
    | Reset of 'v
    | Progress of 'v seq

type internal ComputationResult<'v> = ITramp<Trace * 'v list>

type internal Computation<'v> = 'v context -> ComputationResult<'v>

and 'v context = 'v brick * Store

and 't brick = Brick<'t>

and Brick<'v>(computation : Computation<'v>) as self =
    
    let mutable _comp = computation
    let mutable _trace : IMap<Brick, Versioned> = IMap.empty
    let mutable _alive = false
    let mutable _valid = false
    let mutable _referrer = ISet.empty

    member internal this.valid = _valid
    member internal this.value (store: Store) = 
        match store.tryGet this with
        | (true, v) -> Some (v :?>Versioned<'v>).value
        | _ -> None

    member internal this.instance (store: Store) : 'v option = if _alive then this.value store else None

    interface Brick with
        member this.addReferrer r =
            _referrer <- _referrer.Add r

        member this.removeReferrer r =
            _referrer <- _referrer.Remove r

        member this.invalidate() =
            if _valid then
                _valid <- false
                _referrer |> Seq.iter (fun b -> b.invalidate())

        member this.tryCollect store =
            if (_valid && _referrer.Count = 0) then
                this.invalidate()
                match box ((this.value store) .Value) with
                | :? IDisposable as d -> d.Dispose()
                | _ -> ()
                _trace.Keys |> Seq.iter (fun dep -> dep.removeReferrerAndTryCollect this store)

        member this.versioned store = store.tryGet this |> snd

    member this.evaluate store = (this.evaluateT store).Run()

    member this.evaluateT store : ITramp<'v> = 
        tramp {
            if _valid then
                return (this.value store).Value 
            else
            let! t, c = _comp (self, store)
            // relink referrers (tbd: do this incrementally)
            _trace.Keys |> Seq.iter (fun dep -> dep.removeReferrer self)
            t |> Seq.iter (fun dep -> dep.addReferrer self)

            _trace.Keys |> Seq.iter (fun dep -> dep.tryCollect store)

            _trace <- t |> Seq.map (fun dep -> dep, dep.versioned store) |> IMap.ofSeq

            let newVersion =
                match store.tryGet this with
                | (true, v) -> (v :?> Versioned<'v>).pushSeq c
                | _ -> Versioned<'v>.single (Seq.last c)
                
            store.store this newVersion

            _alive <- true
            _valid <- true
            return newVersion.value
        }

    member this.history (dep : 'd brick) store = 
        tramp {
            let! depV = dep.evaluateT store
            return 
                match _trace.get dep with
                | None -> Reset depV
                | Some (:? Versioned<'d> as v) -> Progress v.tail
                | _ -> failwith "internal error"
        }

    member this.previous (dep : 'd brick) : 'd option = 
        match _trace.get dep with
        | None -> None
        | Some (:? Versioned<'d> as v) -> Some v.head
        | _ -> failwith "internal error"

    member this.write v =
        this.invalidate()
        _comp <- fun _ -> tramp { return [], [v] }

    member this.reset() = 
        this.invalidate()
        _comp <- computation

    member this.invalidate() =
        (this :> Brick).invalidate()    

type 't bricks = seq<'t brick>

let internal makeBrick<'v> f = Brick<'v>(f)

// i want these in a module named Brick

type ManifestMarker<'a> = ManifestMarker of (unit -> 'a)
type SelfValueMarker = SelfValueMarker
type HistoryMarker<'v> = HistoryMarker of Brick<'v>
type PreviousMarker<'v> = PreviousMarker of Brick<'v>

let inline manifest f = ManifestMarker f
let valueOfSelf = SelfValueMarker
let inline historyOf b = HistoryMarker b
let inline previousOf b = PreviousMarker b

let private trampSeq (s: seq<ITramp<'v>>) : ITramp<'v list> =
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


type BrickBuilder() =
    member this.Bind (dependency: 'dep brick, cont: 'dep -> Computation<'next>) : Computation<'next> =
        fun (b, store) ->
            tramp {
                let! depValue = dependency.evaluateT store
                let! contDep, r = cont depValue (b, store)
                return dependency:>Brick :: contDep, r
            }

    member this.Bind (dependencies: 'dep brick seq, cont: 'dep seq -> Computation<'next>) : Computation<'next> =
        fun (b, store) ->
            tramp {
                let! depValues = dependencies |> Seq.map (fun d -> d.evaluateT store) |> trampSeq
                let! contDep, r = cont depValues (b, store)
                let thisDeps = dependencies |> Seq.cast |> Seq.toList
                return thisDeps @ contDep, r
            }

    member this.Bind (ManifestMarker instantiator, cont: 'i -> Computation<'i>) : Computation<'i> =
        fun (b, store as c) ->
            let i = 
                match b.instance store with
                | Some i -> i
                | None -> instantiator()
            cont i c

    member this.Bind (SelfValueMarker, cont: 'v option -> Computation<'v>) : Computation<'v> =
        fun (b, store as c) ->
            let v = b.value store
            cont v c

    member this.Bind (HistoryMarker dep, cont: History<'d> -> Computation<'v>) : Computation<'v> =
        fun (b, store as c) ->
            tramp {
                let! h = b.history dep store
                let! contDep, r = cont h c
                return dep:>Brick :: contDep, r
            }

    member this.Bind (PreviousMarker dep, cont: 'd option -> Computation<'v>) : Computation<'v> =
        fun (b, _ as c) ->
            tramp {
                // note: we don't add the brick to the list of dependencies.
                let p = b.previous dep
                let! contDep, r = cont p c
                return contDep, r
            }

    member this.Return value = fun _ -> tramp { return [], [value] }
    member this.Yield value = this.Return value

    member this.ReturnFrom (brick: 'value brick) = 
        fun (_, store) ->
            tramp {
                let! value = brick.evaluateT store
                return [brick:>Brick], [value]
            }

    member this.YieldFrom (dep: 'dep brick) = this.ReturnFrom dep

    // seeing History as a separate category, we can justify using yield! instead of yield. 
    // yield can not be overloaded.
    member this.ReturnFrom (h: History<'v>) =
        match h with 
        | Reset v -> this.Return v
        | Progress p -> fun _ -> tramp { return [], p |> Seq.toList }

    member this.YieldFrom (h: History<'v>) =
        match h with 
        | Reset v -> this.Yield v
        | Progress p -> fun _ -> tramp { return [], p |> Seq.toList }

    member this.Combine (first: Computation<'v>, second: Computation<'v>) =
        fun b ->
            tramp {
                let! (fd, fv) = first b
                let! (sd, sv) = second b
                return fd @ sd, fv @ sv
            }

    [<CustomOperation("yieldSeq")>]
    member this.Write(nested : Computation<'v>, sequence: 'v seq) =
        fun b ->
            tramp {
                let! (nd, nv) = nested b
                return nd, nv @ (sequence |> Seq.toList)
            }

    member this.Run comp = makeBrick comp

    member this.Delay (f: unit -> Computation<'v>) : Computation<'v> = 
        fun b ->
            f () b

let brick = new BrickBuilder()

(* Transaction

    A transaction can evaluate brick values and set new brick values.

    Inside a transaction, evaluating brick values always resolve to the
    previous state of the brick, never to the values that are changed by 
    the transaction.
*)

type Transaction = unit -> unit

type private Write = unit -> unit
type private TransactionState = Write list
type private TransactionM = TransactionState -> ITramp<TransactionState>

type TransactionBuilder() =

(*
    member this.Bind (brick: 'value brick, cont: 'value -> TransactionM) : TransactionM = 
        fun state ->
            tramp {
                let! value = brick.evaluate()
                let! r = cont value state
                return r
            }
*)

    member this.Zero () = fun f -> tramp { return f }
    member this.Yield _ = fun f -> tramp { return f }

    member this.Run (t : TransactionM): Transaction = 
        fun () -> 
            let t = tramp {
                let! state = t []
                return state |> List.rev |> List.iter (fun w -> w())
            }
            t.Run()

    member this.For(sequence : TransactionM, cont: unit -> TransactionM) : TransactionM =
        fun t ->
            tramp {
                let! t = sequence t
                let! r = cont() t
                return r
            }

    [<CustomOperation("write")>]
    member this.Write(nested : TransactionM, brick: 'v brick, value: 'v) =
        fun t ->
            tramp {
                let! state = nested t
                return (fun () -> brick.write value) :: state
            }
            
    [<CustomOperation("reset")>]
    member this.Reset(nested: TransactionM, brick : 'v Brick) =
        fun t ->
            tramp {
                let! state = nested t
                return (fun () -> brick.reset()) :: state
            }

(* PROGRAM *)

type Program<'v>(root : Brick<'v>) =

    let store = Store()

    let collect() = (root :> Brick).tryCollect store

    interface IDisposable with
        member i.Dispose() = collect()
            
    member this.run() = root.evaluate store

    member this.evaluate (brick: 'x brick) = brick.evaluate store

    member this.valueOf (brick : 'x brick) = if brick.valid then brick.value store else None


    member this.apply(t: Transaction) = 
        t()

type 'v program = Program<'v>

(** BASIC COMBINATORS **)

/// Value brick (tbd: remove in favor of lift?).

let value (v : 'v) : 'v brick =
    brick {
        return v
    }

/// A conversion brick applies a function to a brick.
/// could be a map override?

let convert (c: 's -> 't) (source: 's brick) : 't brick =
    brick {
        let! s = source
        return c s
    }

/// combine two bricks by a funcion.

let combine (c: 'a -> 'b -> 'c) (a: 'a brick) (b: 'b brick) : 'c brick =
    brick {
        let! a = a
        let! b = b
        return c a b
    }

/// lift

type Lifter = Lifter with
   
    static member instance (_:Lifter, v:'v, _:'v brick) : unit -> 'v brick = fun () -> value v
    static member instance (_:Lifter, f: 's -> 't, _:'s brick -> 't brick) = fun () -> convert f
    static member instance (_:Lifter, f: 's -> 'e -> 's, _:'s brick -> 'e brick -> 's brick) =
        fun () ->
            fun (s: 's brick) (e: 'e brick) ->
                brick {
                    let! s = s
                    let! e = e
                    return f s e
                }

let inline lift f = Inline.instance(Lifter,f) ()

// tbd: do we need this anymore?

let liftFolder (f: ('s * 'e) -> 's) =
    fun (s: 's brick, e: 'e brick) ->
        brick {
            let! s = s
            let! e = e
            return f(s,e)
        }

let transaction = new TransactionBuilder()

let toProgram b = new Program<_>(b)
    
[<assembly:AutoOpen("BricksCore")>]
do ()
