// n.b. day 13 has bfs maze solver, day 17 has generalized bfs
// input has 8 locations (0-7)
open System.Collections.Generic

let bfs (isSolution:'a->bool) (getChildren:'a->seq<'a>) (start:'a) =
    let q = new Queue<'a>()
    q.Enqueue(start)
    let rec search() = seq {
        if q.Count > 0 then
            let s = q.Dequeue()
            if isSolution s then 
                yield s
            for c in getChildren s do
                q.Enqueue(c)
            yield! search()
    }
    search()

let shortestFrom maze start =
    let startc = maze start
    let dist = Dictionary<int*int,int>()
    let getChildren ((x,y),d) = 
        [(-1,0);(0,-1);(1,0);(0,1)]
        |> Seq.map (fun (i,j) -> (x+i,y+j) )
        |> Seq.filter (dist.ContainsKey >> not)
        |> Seq.filter (fun p -> maze p <> '#')
        |> Seq.map (fun p -> dist.[p] <- d+1; (p,d+1))
    let isSolution (pos,d) =
        let c = maze pos
        c >= '0' && c < '9' && c <> startc
    bfs isSolution getChildren (start,0) |> Seq.map (fun (p,d) -> maze p,d)
    
let mazeLookup (maze:string[]) (x,y) = maze.[y].[x]
let mazeFind (maze:string[]) c = 
    [for y in 0..maze.Length-1 do 
        for x in 0 ..maze.[y].Length-1 do 
            if maze.[y].[x] = c then yield (x,y)]
            |> List.tryFind (fun _ -> true)

let buildShortestPathLookup maze =
    ['0'..'9'] 
    |> Seq.choose (mazeFind maze)
    |> Seq.collect (fun c -> 
                shortestFrom (mazeLookup maze) c
                |> Seq.map (fun (t,d)-> ((mazeLookup maze c,t),d)))
    |> Map.ofSeq

let findShortestPath maze ret =
    let lookup = buildShortestPathLookup maze
    let rec search (toVisit:char list) current (dist:int) = seq {
        match toVisit with
        | [] ->  yield dist, current
        | _ ->
            for v in toVisit do
                let d = lookup.[(current,v)]
                yield! search (toVisit |> List.filter ((<>) v)) v (dist+d)
    }
    let toVisit = ['1'..'9'] |> List.choose (mazeFind maze) |> List.map (mazeLookup maze)
    let paths = search toVisit '0' 0
    if ret then
        paths |> Seq.map (fun (d,c) -> d + lookup.[c,'0'])
    else
        paths |> Seq.map fst

let maze = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt")
let testMaze = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\testinput.txt")

findShortestPath testMaze false |> Seq.min |> printfn "Test: %d" // 14
findShortestPath maze false |> Seq.min |> printfn "Part a: %d" // 464
findShortestPath maze true |> Seq.min |> printfn "Part b: %d" // 652