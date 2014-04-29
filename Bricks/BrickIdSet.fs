module BrickIdSet

open BrickDefs
open BricksCore
open BrickTime

(* idset
    An id set is a set that organizes data structures that contain a property named id that returns an object.
    That object represents the identity of the data structure and is used to compare instances of it.
*)

type Id = obj

type IdSet<'v> = HashMap<Id, 'v> 

type 'v idset = IdSet<'v>

//
// Helper that collects brick dependencies.
//

type EvaluationContext() = 
    let mutable _deps : Brick list = List.empty

    member this.evaluate (brick: 'a Brick) = 
        let r = brick.evaluate()
        _deps <- (brick :> Brick) :: _deps
        r

    member this.takeDeps() = 
        let r = _deps
        _deps <- List.empty
        r

module IdSet = 
    
    let private bDiff = diff

    let fromSeq (identify: 'v -> Id) (seq: 'v seq) = HashMap.fromSeq (Seq.map (fun v -> identify v, v) seq)

    type Change<'v> =
        | Added of 'v
        | Modified of 'v
        | Removed of 'v

    type ChangeSet<'v> = Change<'v> seq

    let diff (identify: 'v -> Id) (s1:'v idset) (s2:'v idset) = 
        // tbd: combine finding added / modified
        let added = 
            s2.Values 
            |> Seq.filter (fun v -> identify v |> s1.ContainsKey |> not) 
            |> Seq.map Added

        let modified = 
            s2.Values 
            |> Seq.filter (fun v -> let id = identify v in s1.ContainsKey id && (not (obj.ReferenceEquals(s1.[id],v))) )
            |> Seq.map Modified

        let removed = 
            s1.Values 
            |> Seq.filter (fun v -> identify v |> s2.ContainsKey |> not)
            |> Seq.map Removed

        [removed; modified; added] |> Seq.flatten

    let trackChanges (getId: 'v -> Id) (source : 'v idset brick) = 
        source |> bDiff IdSet.Empty (diff getId)

    (* projector
        A set projector projects set differences to three functions. Its main use is to
        project id based incremental set differences to operating system calls.

        Because a changeset brick may result to the same change set when it did not got rebuilt,
        the projector stores the latest changeset and ignores it if it is provided again.
    *)

    type Projector<'s, 't>
        (
            identify: 's -> Id, 
            added: EvaluationContext -> 's -> 't, 
            modified: EvaluationContext -> 's -> 't -> 't, 
            removed: EvaluationContext -> 's -> 't -> unit
        ) =

        let mutable _latest: ChangeSet<'s> option = None
        let mutable _map: HashMap<Id, 't> = HashMap.Empty

        member this.empty = _map.Count = 0
        member this.values = _map.Values

        member this.project changeSet : Brick list = 
            match _latest with
            | Some latest when isSame latest changeSet -> []
            | _ ->
            let depLists = changeSet |> Seq.map this.projectChange |> Seq.flatten
            _latest <- Some changeSet
            depLists |> Seq.toList

        member private this.projectChange change =
            match change with
            | Added s ->
                let ec = EvaluationContext()
                let t = added ec s
                _map <- _map.Add(identify s, t)
                ec.takeDeps()

            | Modified s -> 
                let id = identify s
                let t = _map.[id]
                let ec = EvaluationContext()
                let t = modified ec s t
                _map <- _map.SetItem(id, t)
                ec.takeDeps()

            | Removed s -> 
                let id = identify s
                let t = _map.[id]
                let ec = EvaluationContext()
                removed ec s t
                _map <- _map.Remove id
                ec.takeDeps()
