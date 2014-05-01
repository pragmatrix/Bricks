module Bricks

open BrickDefs
open System.Collections.Immutable

type HashMap = BrickDefs.HashMap
type HashMap<'key, 'value> = BrickDefs.HashMap<'key, 'value>
type ImmutableDictionary<'k, 'v> with
    member inline this.get k = BrickDefs.Map.get this k
    member inline this.has k = BrickDefs.Map.has this k

module Set =
    let inline ofSeq s = BrickSet.Set.ofSeq s
    let inline diff s1 s2 = BrickSet.Set.diff s1 s2
    let inline Added v = BrickSet.Set.Added v
    let inline Removed v = BrickSet.Set.Removed v

type 'v brick = BricksCore.Brick<'v>
type 't bricks = seq<BricksCore.Brick<'t>>

type Transaction = BricksCore.Transaction
type Program<'v> = BricksCore.Program<'v>

let brick = BricksCore.brick
let transaction = BricksCore.transaction

let inline value v = BricksCore.value v
let inline convert a b = BricksCore.convert a b
let inline valueOf b = BricksCore.valueOf b
let inline manifest f = BricksCore.manifest f       

type 'v set = 'v BrickDefs.set
type b = BrickDefs.b

let inline toProgram b = BricksCore.toProgram b

