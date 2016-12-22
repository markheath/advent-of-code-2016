open System.Text.RegularExpressions
open System.Collections.Generic

let (|Regex|_|) ptrn str = match Regex.Match(str, ptrn) with m when m.Success -> Some([ for g in m.Groups -> g.Value ] |> List.tail) | _ -> None

let parseLine line =
    match line with
    | Regex @"/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+\d+\%" [x;y;s;u;a] -> Some ((int x,int y),int s,int u,int a)
    | _ -> None

let input = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt")
 
let nodes = input |> Array.choose parseLine

seq {for (pos,s,u,a) in nodes do
        for (pos',s',u',a') in nodes do
            if pos <> pos' && u <> 0 && u < a' then
                yield 1} |> Seq.length // part a: 960

type Node = { Size:int; Avail: int; Used:int }
type State = { DataPos: int*int; State: Map<int*int,Node>; Moves:int}

let isSolution state = state.DataPos = (0,0)

let getChildren state = seq {
    let canMove src dst =
        state.State.[src].Used <= state.State.[dst].Avail
    let x,y = state.pos
    if x > 0 && canMove state.pos (x-1,y) then
        yield
}