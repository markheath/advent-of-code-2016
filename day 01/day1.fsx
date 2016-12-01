open System
type Direction = North | East | South | West
type Turn = Right | Left
type Instruction = Turn of direction : Turn | Move of int
type State = { position: int * int; facing: Direction; visited: Set<int*int> }

//let input = "L3, R1, L4, L1, L2, R4, L3, L3, R2, R3, L5, R1, R3, L4, L1, L2, R2, R1, L4, L4, R2, L5, R3, R2, R1, L1, L2, R2, R2, L1, L1, R2, R1, L3, L5, R4, L3, R3, R3, L5, L190, L4, R4, R51, L4, R5, R5, R2, L1, L3, R1, R4, L3, R1, R3, L5, L4, R2, R5, R2, L1, L5, L1, L1, R78, L3, R2, L3, R5, L2, R2, R4, L1, L4, R1, R185, R3, L4, L1, L1, L3, R4, L4, L1, R5, L5, L1, R5, L1, R2, L5, L2, R4, R3, L2, R3, R1, L3, L5, L4, R3, L2, L4, L5, L4, R1, L1, R5, L2, R4, R2, R3, L1, L1, L4, L3, R4, L3, L5, R2, L5, L1, L1, R2, R3, L5, L3, L2, L1, L4, R4, R4, L2, R3, R1, L2, R1, L2, L2, R3, R3, L1, R4, L5, L3, R4, R4, R1, L2, L5, L3, R1, R4, L2, R5, R4, R2, L5, L3, R4, R1, L1, R5, L3, R1, R5, L2, R1, L5, L2, R2, L2, L3, R3, R3, R1"
let input = System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + "\\input.txt")
let parseInstruction (i:string) = 
    let turn = match i.[0] with | 'L' -> Left | 'R' -> Right | _ -> failwith "parseError"
    let blocks = int(i.Substring(1))
    [| Turn turn; Move blocks |]
let instructions = input.Split([|", "|], StringSplitOptions.None) |> Array.collect parseInstruction

(* alternatively could have used regex, e.g. 
open System.Text.RegularExpressions
Regex.Matches(input, "[RL]\d+") 
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Seq.toArray
*)
let rotate start direction = 
    let directions = [|North;East;South;West|]
    let offset = if direction = Left then 3 else 1
    directions.[(Array.IndexOf (directions,start)+offset) % 4]

let move state instruction =
    match instruction with 
    | Turn direction ->
        { state with facing = rotate state.facing direction }
    | Move blocks ->
        let x,y = state.position
        let newPos = match state.facing with
                        | North -> (x, y - blocks)
                        | West -> (x - blocks, y)
                        | South -> (x, y + blocks)
                        | East -> (x + blocks, y)
        { state with position = newPos; visited = Set.add state.position state.visited  }
       
let startState = { position = (0,0); facing = North; visited = Set.empty } 

let endState = instructions |> Array.fold move startState

let getDistance (x,y) = abs(x) + abs(y)

printfn "Part a: Distance from home is %d" (getDistance endState.position)
// my answer was 252

let expandInstruction i =
    match i with
    | Turn direction -> [ Turn direction ]
    | Move blocks -> [ for n in 1..blocks -> Move 1 ]

let visitedTwice = instructions 
                    |> Seq.collect expandInstruction 
                    |> Seq.scan move startState
                    |> Seq.find (fun f -> f.visited.Contains(f.position))
                    
printfn "Part b: Distance from home is %d" (getDistance visitedTwice.position)
// my answer was 143

