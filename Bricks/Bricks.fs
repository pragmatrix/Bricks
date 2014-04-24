module Bricks

open System.Collections.Immutable

type HashMap = BrickDefs.HashMap
type HashMap<'key, 'value> = BrickDefs.HashMap<'key, 'value>

type Program = BricksCore.Program
type 'v brick = BricksCore.Brick<'v>
type Transaction = BricksCore.Transaction

type ImmutableDictionary<'k, 'v> with
    member inline this.get k = BrickDefs.map.get this k
    member inline this.has k = BrickDefs.map.has this k

type 't bricks = seq<BricksCore.Brick<'t>>

module bset =
    let inline ofSeq s = BrickSet.bset.ofSeq s
    let inline diff s1 s2 = BrickSet.bset.diff s1 s2
    let inline Added v = BrickSet.bset.Added v
    let inline Removed v = BrickSet.bset.Removed v

let brick = BricksCore.brick
let program = BricksCore.program
let transaction = BricksCore.transaction

let inline value v = BricksCore.value v
let inline convert a b = BricksCore.convert a b
let inline valueOf b = BricksCore.valueOf b
let inline manifest f = BricksCore.manifest f
let inline memo def source = BricksCore.memo def source

type EvaluationContext = BricksCore.EvaluationContext

type 'e idset = BricksCore.IdSet<'e>

module IdSet = 
    type Projector<'a, 'b> = BricksCore.IdSet.Projector<'a, 'b>
    let inline trackChanges i s = BricksCore.IdSet.trackChanges i s
    let inline fromSeq i s = BricksCore.IdSet.fromSeq i s
