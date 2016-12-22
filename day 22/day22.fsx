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
                yield 1} |> Seq.length |> printfn "part a: %d" // 960

let printPos (pos,s,u,a) =
    if u = 0 then "_" elif u > 100 then "#" else "."

nodes |> Array.chunkBySize 30 |>
    Array.map (Array.map printPos >> String.concat " ")
    |> (fun a -> System.IO.File.WriteAllLines(__SOURCE_DIRECTORY__ + "\\grid.txt", a))

// from visual inspection, (grid is 30x33)
// move empty 12 UP, 29 LEFT, 28 DOWN
// move data 32 UP (but shuffle hole around is 4 more moves) : (31*5)+1
12+29+28+(31*5)+1 |> printfn "part b: %d" //225
