module BricksCore

(** BRICKS, F# COMPUTATION EXPRESSIONS AND COMBINATORS FOR LAZY INCREMENTAL COMPUTATION, SEE README.MD **)

open System
open System.Diagnostics
open System.Collections.Immutable
open System.Collections.Generic
open System.Linq

open BrickCollections
open InlineHelper
open Trampoline

(** BRICK, ENVIRONMENT **)

#nowarn "0346" // for Beacon.GetHashCode()

[<CustomEquality>][<CustomComparison>]
type Beacon = { mutable valid: bool }
    with
        interface IComparable with
            member this.CompareTo other =
                this.GetHashCode().CompareTo (other.GetHashCode())

        override this.Equals(other) =
            Object.ReferenceEquals(this, other)

        member this.invalidate() = this.valid <- false

        static member create = { valid = false }

type Tick = int64

type Versioned = interface
        abstract member value : (Tick * obj)
        abstract member tail : Versioned seq
    end


[<AutoOpen>]
module VersionedExtensions = 

    type Versioned with
        member this.last =
            let t = this.tail
            if Seq.isEmpty t then
                this
            else 
                Seq.last t

type BeaconNode = 
    | Mutable of Beacon
    | Computed of Beacon * BeaconNode list

and Brick =
    abstract member versioned : Versioned with get
    abstract member node : BeaconNode
    abstract member evaluateT: unit -> ITramp<obj>

type History<'v> = 
    | Reset of 'v
    | Progress of 'v seq

type 'v brick = 
    inherit Brick
    abstract member evaluateT: unit -> ITramp<'v>
    abstract member value: 'v option
    abstract member history: 'd brick -> ITramp<History<'d>>
    abstract member previous: 'd brick -> 'd option

type Mutable<'v> = 
    inherit brick<'v>
    abstract member write: Tick -> 'v -> unit
    abstract member reset: Tick -> unit

let private updateNode node = 

    // tbd: tramp or use tailcalls to save stack space

    let rec un node notify = 
        match node with
        | Mutable b -> if not b.valid then notify()
        | Computed (b, deps) ->
        if not b.valid then notify() 
        else
        let n() =
            if (b.valid) then
                b.valid <- false
                notify()

        deps |> List.iter (fun d -> un d n)

    un node id

[<AutoOpen>]
module BrickExtensions =

    type 'v brick with
        member this.evaluate() =
            updateNode this.node
            this.evaluateT().Run()

        member this.valid = 
            match this.node with
            | Mutable b -> b.valid
            | Computed (b, _) -> b.valid

type Versioned<'v> = { value: (Tick * 'v); mutable next: Versioned<'v> option } with

    member private this.boxedValue = fst this.value, snd this.value |> box

    interface Versioned with
        member this.value = this.boxedValue
        member this.tail = this.tail |> Seq.map (fun v -> v :> Versioned)

    static member single (tick: Tick, value: 'v) = { value = (tick, value); next = None }

    member this.push (time: Tick, value: 'v) =
        assert(this.next.IsNone)
        let n = Versioned<'v>.single (time, value)
        this.next <- Some n
        n

    member this.head = this

    member this.tail =
        this.next |> Seq.unfold (Option.map (fun n -> n, n.next))
    
    member this.pushSeq values = 
        values |> Seq.fold (fun (c : Versioned<_>) v -> c.push v) this

(* Global Tick counter *)

let mutable private CurrentTick = 0L

let internal newTick() = 
    let newT = CurrentTick + 1L
    CurrentTick <- newT
    newT

(** Mutable Value **)

type MutableBrick<'v>(tick: Tick, initial: 'v) =

    let mutable _versioned = Versioned<'v>.single(tick, initial)

    let beacon = Beacon.create
    let node = Mutable beacon

    let eval = 
        tramp {
            beacon.valid <- true
            return _versioned.value
        }

    interface Mutable<'v> with

        member this.versioned = _versioned :> _
        member this.value = _versioned.value |> snd |> Some

        member this.history (_ : 'd brick) = 
            failwith "internal error"

        member this.previous (_ : 'd brick) : 'd option = 
            failwith "internal error"

        member this.evaluateT() : ITramp<obj> = eval |> Trampoline.map (fun v -> snd v |> box)

        member this.evaluateT() : ITramp<'v> = eval |> Trampoline.map snd

        member this.write tick v =
            _versioned <- _versioned.push (tick, v)
            beacon.invalidate()

        member this.reset tick =
            _versioned <- _versioned.push (tick, initial)
            beacon.invalidate() 

        member this.node = node

(** SIGNAL **)

type SignalBrick<'v>(dependencies: Brick list, processor: obj array -> 'v) =

    let numParams = List.length dependencies
    let beacon = Beacon.create
    let mutable _node = Computed (beacon, [])
    let mutable _versioned : Versioned<'v> option = None

    let mutable _arguments : obj array = [||]
    let mutable _latest : Versioned array option = None

    let proc (values : (int * (Tick * obj)) list) =

        let isSet = Array.create numParams false
        let clear() = Array.fill isSet 0 numParams false
        
        let tickOf (_, (t, _)) = t
        
        let rec p tick mustProcess soFar todo = 
            match todo with
            | [] -> 
                if mustProcess then
                    let res = processor _arguments
                    (tick, res) :: soFar 
                else
                    soFar
                |> List.rev
            | (i, (t, v) as next) :: rest ->
                let canUse = t = tick && not isSet.[i]
                if canUse then 
                    _arguments.[i] <- v
                    isSet.[i] <- true
                    p tick true soFar rest
                else
                    assert (mustProcess)
                    let res = processor _arguments
                    clear()
                    let nextTick = tickOf next
                    p nextTick false ((tick, res)::soFar) todo
                    
        match values with
        | [] -> []
        | (first::_) -> p (tickOf first) false [] values
    
    let eval = 
        tramp {
            // even if we don't use the values directly, we need to run evaluateT to update the versioned chains
            let! _ = dependencies |> Seq.map (fun d -> d.evaluateT()) |> trampSeq
            let versioned = dependencies |> Seq.map (fun d -> d.versioned)

            // we are processing tuples of tick, value index, value

            let toProcess = 
                match _latest with
                | None -> 
                    // initialize the value array to the latest values
                    _arguments <-
                        versioned 
                        |> Seq.map (fun v -> v.value |> snd)
                        |> Seq.toArray

                    versioned 
                    |> Seq.mapi (fun i v -> i, v.value)

                | Some latest -> 
                    latest 
                    |> Seq.mapi (fun i v -> i, v.tail)
                    |> Seq.collect 
                        (fun (i, vs) -> 
                            vs 
                            |> Seq.map (fun v -> i, v.value))

            _latest <-
                versioned
                |> Seq.map (fun v -> v.last)
                |> Seq.toArray
                |> Some

            let orderedByTick = 
                toProcess |>
                Seq.sortBy (fun v -> v |> snd |> fst)

            let res = proc (orderedByTick |> Seq.toList)

            // note that nodes might change!
            let depNodes = dependencies |> Seq.map (fun d -> d.node)
            _node <- Computed (beacon, depNodes |> Seq.toList)

            _versioned <-        
                match _versioned with
                | None -> Versioned<'v>.single (Seq.last res)
                | Some v -> v.pushSeq res
                |> Some

            beacon.valid <- true
            return _versioned.Value.value
        }

    interface 'v brick with

        member this.versioned = _versioned.Value :> Versioned
        member this.value = _versioned |> Option.map(fun v -> snd v.value) 

        member this.history (_ : 'd brick) = 
            failwith "internal error"

        member this.previous (_ : 'd brick) : 'd option = 
            failwith "internal error"

        member this.evaluateT() : ITramp<obj> = eval |> Trampoline.map (snd >> box)
        member this.evaluateT() : ITramp<'v> = eval |> Trampoline.map snd

        member this.node = _node


(** COMPUTED **)

type internal Trace = Brick list

type internal ComputationResult<'v> = ITramp<Trace * 'v list>

type internal Computation<'v> = 'v brick -> ComputationResult<'v>

and ComputedBrick<'v>(computation : Computation<'v>) as self =
    
    let beacon = Beacon.create
    let mutable _node = Computed (beacon, [])
    let mutable _trace : IMap<Brick, Versioned> = IMap.empty
    let mutable _versioned : Versioned<'v> option = None

    let eval = 
        tramp {
            if beacon.valid then
                return _versioned.Value.value 
            else
            let! t, c = computation self
            // tbd: combine the following queries
            _trace <- 
                t
                |> Seq.map (fun dep -> dep, dep.versioned) 
                |> IMap.ofSeq

            let resultTick =
                if t = [] then CurrentTick else
                t 
                |> Seq.map (fun dep -> dep.versioned.value |> fst) 
                |> Seq.max

            let depNodes = 
                t 
                |> Seq.map (fun dep -> dep.node) 
                |> Seq.toList

            _node <- Computed (beacon, depNodes)

            _versioned <-        
                match _versioned with
                | None -> Versioned<'v>.single (resultTick, (Seq.last c))
                | Some v -> v.pushSeq (c |> Seq.map (fun c -> (resultTick, c)))
                |> Some

            beacon.valid <- true
            return _versioned.Value.value
        }

    interface 'v brick with

        member this.versioned = _versioned.Value :> Versioned
        member this.value = _versioned |> Option.map(fun v -> snd v.value) 

        member this.history (dep : 'd brick) = 
            tramp {
                let! depV = dep.evaluateT()
                return 
                    match _trace.get dep with
                    | None -> Reset depV
                    | Some (:? Versioned<'d> as v) -> Progress (v.tail |> Seq.map(fun v -> v.value |> snd) |> Seq.toList)
                    | _ -> failwith "internal error"
            }

        member this.previous (dep : 'd brick) : 'd option = 
            match _trace.get dep with
            | None -> None
            | Some (:? Versioned<'d> as v) -> Some (snd v.head.value)
            | _ -> failwith "internal error"

        member this.evaluateT() : ITramp<obj> = eval |> Trampoline.map (snd >> box)
        member this.evaluateT() : ITramp<'v> = eval |> Trampoline.map snd

        member this.node = _node

type 't bricks = seq<'t brick>

let internal makeBrick<'v> f = ComputedBrick<'v>(f) :> 'v brick

// i want these in a module named Brick

type SelfValueMarker = SelfValueMarker
type HistoryMarker<'v> = HistoryMarker of 'v brick
type PreviousMarker<'v> = PreviousMarker of 'v brick

let valueOfSelf = SelfValueMarker
let inline historyOf b = HistoryMarker b
let inline previousOf b = PreviousMarker b

type BrickBuilder() =
    member this.Bind (dependency: 'dep brick, cont: 'dep -> Computation<'next>) : Computation<'next> =
        fun b ->
            tramp {
                let! depValue = dependency.evaluateT()
                let! contDep, r = cont depValue b
                return dependency:>Brick :: contDep, r
            }

    member this.Bind (dependencies: 'dep brick seq, cont: 'dep seq -> Computation<'next>) : Computation<'next> =
        fun b ->
            tramp {
                let! depValues = dependencies |> Seq.map (fun d -> d.evaluateT()) |> trampSeq
                let! contDep, r = cont depValues b
                let thisDeps = dependencies |> Seq.cast |> Seq.toList
                return thisDeps @ contDep, r
            }

    member this.Bind (SelfValueMarker, cont: 'v option -> Computation<'v>) : Computation<'v> =
        fun b ->
            let v = b.value
            cont v b

    member this.Bind (HistoryMarker dep, cont: History<'d> -> Computation<'v>) : Computation<'v> =
        fun b ->
            tramp {
                let! h = b.history dep
                let! contDep, r = cont h b
                return dep:>Brick :: contDep, r
            }

    member this.Bind (PreviousMarker dep, cont: 'd option -> Computation<'v>) : Computation<'v> =
        fun b ->
            tramp {
                // note: we don't add the brick to the list of dependencies.
                let p = b.previous dep
                let! contDep, r = cont p b
                return contDep, r
            }

    member this.Return value = fun _ -> tramp { return [], [value] }
    member this.Yield value = this.Return value
    
    member this.ReturnFrom (brick: 'value brick) = 
        fun _ ->
            tramp {
                let! value = brick.evaluateT();
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

type private Write = Tick -> unit
type private TransactionState = Write list
type private TransactionM = TransactionState -> ITramp<TransactionState>

type TransactionBuilder() =
    member this.Bind (brick: 'value brick, cont: 'value -> TransactionM) : TransactionM = 
        fun state ->
            tramp {
                let! value = brick.evaluateT()
                let! r = cont value state
                return r
            }

    member this.Zero () = fun f -> tramp { return f }
    member this.Yield _ = fun f -> tramp { return f }

    member this.Run (t : TransactionM): Transaction = 
        fun () -> 
            let t = tramp {
                let! state = t []
                let tick = newTick()
                return state |> List.rev |> List.iter (fun w -> w tick)
            }
            t.Run()

    member this.For(seq : TransactionM, cont: unit -> TransactionM) : TransactionM =
        fun t ->
            tramp {
                let! t = seq t
                let! r = cont() t
                return r
            }

    [<CustomOperation("write")>]
    member this.Write(nested : TransactionM, brick: Mutable<'v>, value: 'v) =
        fun t ->
            tramp {
                let! state = nested t
                return (fun time -> brick.write time value) :: state
            }
            
    [<CustomOperation("reset")>]
    member this.Reset(nested: TransactionM, brick : Mutable<'v>) =
        fun t ->
            tramp {
                let! state = nested t
                return (fun time -> brick.reset time) :: state
            }

(* PROGRAM *)

type 'v program(root : 'v brick) =
            
    interface IDisposable with
        member this.Dispose() = ()

    member this.run() = root.evaluate()

    member this.apply(t: Transaction) = t()

(** LIFT for continous functions **)

type Lifter = Lifter with
   
    static member instance (_:Lifter, v:'v, _:Mutable<'v>) : unit -> Mutable<'v> = 
        fun () -> 
            MutableBrick<'v>(CurrentTick, v) :> _

    static member instance (_:Lifter, f: 's -> 't, _:'s brick -> 'r brick) = 
        fun () -> 
            fun s ->
                brick {
                    let! s = s
                    return f s
                }

    static member instance (_:Lifter, f: 's1 -> 's2 -> 't, _:'s1 brick -> 's2 brick -> 'r brick) =
        fun () ->
            fun s1 s2 ->
                brick {
                    let! s1 = s1
                    let! s2 = s2
                    return f s1 s2
                }

    static member instance (_:Lifter, f: 's1 -> 's2 -> 's3 -> 't, _:'s1 brick -> 's2 brick -> 's3 brick -> 'r brick) =
        fun () ->
            fun s1 s2 s3 ->
                brick {
                    let! s1 = s1
                    let! s2 = s2
                    let! s3 = s3
                    return f s1 s2 s3
                }

let inline lift f = Inline.instance(Lifter, f) ()

(* Signal lifting *)

let signal v = MutableBrick<'v>(CurrentTick, v) :> Mutable<'v>

let inline signal1 (f : 's -> 'r) (s: 's brick) = 
    let p (a: obj array) = f (unbox a.[0])
    SignalBrick<_>([s], p) :> _ brick

let inline signal2 (f : 's1 -> 's2 -> 'r) (s1 : 's1 brick) (s2: 's2 brick) =
    let p (a: obj array) = f (unbox a.[0]) (unbox a.[1])
    SignalBrick<_>([s1;s2], p) :> 'r brick

let inline signal3 (f : 's1 -> 's2 -> 's3 -> 'r) (s1 : 's1 brick) (s2: 's2 brick) (s3: 's3 brick) =
    let p (a: obj array) = f (unbox a.[0]) (unbox a.[1]) (unbox a.[2])
    SignalBrick<_>([s1;s2;s3], p) :> 'r brick

(* foldp, Elm inspired *)

let foldp (f: 's -> 'v -> 's) (initial: 's) (source: 'v brick) =
    let state = ref initial
    let folder value =
        let s = f (!state) value
        state := s
        !state

    signal1 folder source

let transaction = new TransactionBuilder()

let valueOf (brick : 'v brick) = if brick.valid then brick.value else None
let toProgram b = new (_ program)(b)
    
[<assembly:AutoOpen("BricksCore")>] ()
