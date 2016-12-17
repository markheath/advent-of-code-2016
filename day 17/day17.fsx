open System.Text
open System
open System.Collections.Generic
let md5 = System.Security.Cryptography.MD5.Create()
let hash (s:string) =
    Encoding.Default.GetBytes(s)
    |> md5.ComputeHash
    |> (fun h -> BitConverter.ToString(h).ToLower().Replace("-",""))

let bfs (isSolution:'a->bool) (getChildren:'a->seq<'a>) (start:'a) =
    let q = new Queue<'a>()
    q.Enqueue(start)
    let rec search() = seq {
        if q.Count > 0 then
            let s = q.Dequeue()
            if isSolution s then 
                yield s
            else
                for c in getChildren s do
                    q.Enqueue(c)
            yield! search()
    }
    search()

type State = { pos: int*int; path:string}

let isSolution state = state.pos = (3,3)

let isOpen (c:char) = c >= 'b' && c <= 'f'

let getValidMoves passcode state = seq {
    let (x,y) = state.pos
    let h = hash (passcode + state.path)
    if isOpen h.[0] && y > 0 then
        yield { pos = (x,y-1); path = state.path + "U" }
    if isOpen h.[1] && y < 3 then
        yield { pos = (x,y+1); path = state.path + "D" }
    if isOpen h.[2] && x > 0 then
        yield { pos = (x-1,y); path = state.path + "L" }
    if isOpen h.[3] && x < 3 then
        yield { pos = (x+1,y); path = state.path + "R" }
}
let startState = { pos=(0,0); path="" }
let solve passcode =    
    let s = bfs isSolution (getValidMoves passcode) startState |> Seq.head
    s.path 

getValidMoves "hijkl" startState |> Seq.toArray |> printfn "Test %A" // down only
getValidMoves "hijkl" { pos=(0,1); path="D"} |> Seq.toArray |> printfn "Test %A" // up or right
getValidMoves "hijkl" { pos=(1,1); path="DR"} |> Seq.toArray |> printfn "Test %A" // all doors closed
getValidMoves "hijkl" { pos=(1,1); path="DR"} |> Seq.toArray |> printfn "Test %A" // all doors closed
let assertEquals actual expected =
    if actual = expected then
        printfn "PASS %A" actual
    else 
        printfn "FAIL: got %A expected %A" actual expected

assertEquals (solve "ihgpwlah") "DDRRRD"
assertEquals (solve "kglvqrro") "DDUDRLRRUDRD"
assertEquals (solve "ulqzkmiv") "DRURDRUDDLLDLUURRDULRLDUUDDDRR"
solve "pxxbnzuo" |> printfn "Part a: %s" // not 10, RDULRDDRRD

let solveB passcode =    
    bfs isSolution (getValidMoves passcode) startState 
        |> Seq.map (fun s -> s.path.Length)
        |> Seq.max 

assertEquals (solveB "ihgpwlah") 370
assertEquals (solveB "kglvqrro") 492
assertEquals (solveB "ulqzkmiv") 830
solveB "pxxbnzuo" |> printfn "Part b: %d" // 752

bfs isSolution (getValidMoves "ihgpwlah") startState 