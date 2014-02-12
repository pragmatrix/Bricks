module BricksUI

open Bricks


type Window = { id: string; title:string; width: int; height: int}

type Test = {id: string }

let inline idOf< ^T when ^T:(member id: string)> o = (^T : (member id:string) o)

let idOfWindow (w:Window) = idOf w
let idOfTest (w:Test) = idOf w


type Application = Window set

let run (appBrick:Application brick) = 
    program {
        let! application = appBrick
    }
    