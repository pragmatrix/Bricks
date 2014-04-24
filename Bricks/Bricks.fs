module Bricks

open System.Collections.Immutable

type HashMap = BrickDefs.HashMap
type HashMap<'key, 'value> = BrickDefs.HashMap<'key, 'value>
type ImmutableDictionary<'k, 'v> with
    member inline this.get k = BrickDefs.map.get this k
    member inline this.has k = BrickDefs.map.has this k

module bset =
    let inline ofSeq s = BrickSet.bset.ofSeq s
    let inline diff s1 s2 = BrickSet.bset.diff s1 s2
    let inline Added v = BrickSet.bset.Added v
    let inline Removed v = BrickSet.bset.Removed v

type 'v brick = BricksCore.Brick<'v>
type 't bricks = seq<BricksCore.Brick<'t>>

type Transaction = BricksCore.Transaction
type Program = BricksCore.Program

let brick = BricksCore.brick
let program = BricksCore.program
let transaction = BricksCore.transaction

let inline value v = BricksCore.value v
let inline convert a b = BricksCore.convert a b
let inline valueOf b = BricksCore.valueOf b
let inline manifest f = BricksCore.manifest f
let inline memo def source = BricksCore.memo def source

type EvaluationContext = BrickIdSet.EvaluationContext

type 'e idset = BrickIdSet.IdSet<'e>

module IdSet = 
    type Projector<'a, 'b> = BrickIdSet.IdSet.Projector<'a, 'b>
    let inline trackChanges i s = BrickIdSet.IdSet.trackChanges i s
    let inline fromSeq i s = BrickIdSet.IdSet.fromSeq i s
