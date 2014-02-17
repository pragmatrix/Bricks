module Bricks

open System
open System.Diagnostics
open System.Collections.Immutable
open System.Collections.Generic

(* *)

type HashSet = ImmutableHashSet
type HashSet<'k> = ImmutableHashSet<'k>
type 'k set = HashSet<'k>
module set = 
    let empty<'e> = HashSet<'e>.Empty
    let ofSeq = HashSet.CreateRange

type HashMap = ImmutableDictionary
type HashMap<'k, 'v> = ImmutableDictionary<'k, 'v>

module List =
    let flatten l = List.collect id l

(* Brick *)

type Brick =
    abstract member invalidate: unit -> Brick list
    abstract member write: obj option -> unit
    abstract member addReferrer: Brick -> unit
    abstract member removeReferrer: Brick -> unit

let rec private invalidate (bricks: Brick list) = 
    match bricks with
    | [] -> ()
    | brick::rest -> 
    let referrer = brick.invalidate()
    invalidate (referrer @ rest)

let private commitWrites writes =
    writes |> List.iter (fun (b:Brick, value) ->
        let env = invalidate [b]
        b.write value
    )

type ComputationResult<'v> = { trace: Brick list; value: 'v; }

type Computation<'v> = unit -> ComputationResult<'v>

type Brick<'v>(f : Computation<_>) =

    let mutable trace: Brick list = []
    let mutable value: 'v option = None
    let mutable referrer: Brick set = set.empty

    member this.Trace: Brick list = trace
    member this.Value: 'v option = value
    member this.Referrer: Brick set = referrer

    member private this.addTrace t = 
        t |> List.iter (fun (b:Brick) -> b.addReferrer this)        
        trace <- t

    member private this.removeTrace() =
        trace |> List.iter (fun (b:Brick) -> b.removeReferrer this)
        trace <- []

    member this.evaluate() =  
        match value with
        | Some value -> value
        | None ->
        assert (trace = [] && referrer.Count = 0)
        let res = f()
        value <- Some res.value
        this.addTrace res.trace
        res.value

    abstract member invalidating : 'v -> unit
    default this.invalidating v = ()

    interface Brick with
        member this.invalidate() =
            if (value.IsNone) then List.empty else
            this.invalidating value.Value
            value <- None
            this.removeTrace()
            let res = referrer |> Seq.toList
            referrer <- set.empty
            res
        member this.write v = 
            assert (value.IsNone && trace = [] && referrer.Count = 0)
            match v with
            | Some v -> value <- Some (v :?> 'v)
            | None -> value <- None

        member this.addReferrer brick = 
            referrer <- referrer.Add brick

        member this.removeReferrer brick = 
            referrer <- referrer.Remove brick

type 't brick = Brick<'t>



let private makeBrick f = Brick<_>(f)

type BrickBuilder() =
    (* tbd: instead of chaining the computation expression internal closure bricks, we could combine them *)
    member this.Bind (dependency: 'dep brick, cont: 'dep -> 'next brick) : 'next brick =
        fun () ->
            let depValue = dependency.evaluate();
            let contBrick = cont depValue
            let value = contBrick.evaluate()
            { trace = [dependency]; value = value }
        |> makeBrick

    (* need to wrap this in a new brick here, to allow a shadow write later on *)
    member this.ReturnFrom (brick: 'value brick) = 
        fun () ->
            let value = brick.evaluate();
            { trace = [brick]; value = value }
        |> makeBrick

    member this.Return value = 
        fun () -> { trace = []; value = value }
        |> makeBrick

let brick = new BrickBuilder()

(* Program *)

type Write = Brick * (obj option)

type Program() = 
    member this.evaluate (brick: 'v brick) : 'v = 
        brick.evaluate()

    member this.invalidate (bricks: Brick list) =
        invalidate bricks

    static member empty = Program()


(* Transaction

    A transaction can evaluate brick values and set new brick values.

    Inside a transaction, evaluating brick values always resolve to the
    previous state of the brick, never to the values that are changed by 
    the transaction.
*)

type Transaction = Write list
type TransactionM = Transaction -> Transaction

type TransactionBuilder() =
    member this.Bind (brick: 'value brick, cont: 'value -> TransactionM) : TransactionM = 
        fun wl ->
            let value = brick.evaluate()
            cont value wl

    member this.Zero () = id
    member this.Yield _ = id

    member this.Run (t : TransactionM): (unit -> unit) = 
        fun () -> 
            let wl = t []
            wl |> List.rev |> commitWrites

//    member this.Return p = p

    member this.For(seq : TransactionM, cont: unit -> TransactionM) : TransactionM =
        fun t ->
            let t = seq t
            cont() t

    [<CustomOperation("write", MaintainsVariableSpace = true)>]
    member this.Write(nested : TransactionM, brick: 'v brick, value: 'v) =
        fun t ->
            let wl = nested t
            (brick :> Brick, Some (value :> obj)) :: wl
            
    [<CustomOperation("reset", MaintainsVariableSpace = true)>]
    member this.Reset(nested: TransactionM, brick : Brick) =
        fun t ->
            let wl = nested t
            (brick, None) :: wl

let transaction = new TransactionBuilder()

type ProgramM = Program -> Program

type ProgramBuilder() =

    (* A regular let! is the isolated evaluation of a root brick in the context of the program *)

    member this.Bind (brick: 'value brick, cont: 'value -> ProgramM) : ProgramM = 
        fun p ->
            let v = p.evaluate brick
            cont v p

    member this.Zero () = id
    member this.Yield _ = id

    member this.For(seq : ProgramM, cont: unit -> ProgramM) : ProgramM =
        fun p ->
            let p = seq p
            cont() p

    [<CustomOperation("apply", MaintainsVariableSpace = true)>]
    member this.Apply(nested : ProgramM, transaction: (unit -> unit)) = 
        fun p ->
            let p = nested p
            transaction()
            p

let program = new ProgramBuilder()

(* Memo 

    A memo brick is a brick that is wrapped around another brick and remembers the previous value of it.

    The value of a memo brick is (previousValue, currentValue)
*)

let memo (target:'v brick) (def:'v) : ('v * 'v) brick = 
    let previous = ref def
    Brick<('v*'v)>(fun () ->
        let v = target.evaluate()
        let res = { trace = [target]; value = (!previous, v)}
        previous := v
        res
    )

(* idset
    An id set is a set that organizes data structures that contain a property named id that returns an object.
    That object represents the identity of the data structure and is used to compare instances of it.
*)

type IdSet<'v when 'v:(member id:obj)> = HashMap<obj, 'v> 

type 'v idset when 'v:(member id:obj) = IdSet<'v>

module IdSet = 
    type Diff<'v> = { added: 'v seq; removed: 'v seq; modified: 'v seq }

    let inline id v = (^v: (member id:obj) v)

    let inline diff (s1:'v idset) (s2:'v idset) = 
        // tbd: combine finding added / modified
        let added = s2.Values |> Seq.filter (fun v -> id v |> s1.ContainsKey |> not)
        let removed = s1.Values |> Seq.filter (fun v -> id v |> s2.ContainsKey |> not)
        let modified = s2.Values |> Seq.filter (fun v -> let id = id v in s1.ContainsKey id && (not (obj.ReferenceEquals(s1.[id],v))) )
        { added = added; removed = removed; modified = modified}
