open System
type Direction = North | East | South | West
type Turn = Right | Left
type Instruction = Turn of direction : Turn | Move of int
type State = { position: int * int; facing: Direction; visited: Set<int*int> }

let input = System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + "\\input.txt")
let parseInstruction (i:string) = 
    let turn = match i.[0] with | 'L' -> Left | 'R' -> Right | _ -> failwith "parseError"
    let blocks = int(i.Substring(1))
    [| Turn turn; Move blocks |]
let instructions = input.Split([|", "|], StringSplitOptions.None) |> Array.collect parseInstruction

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

printfn "Part a: Distance from home is %d" (getDistance endState.position) // my answer was 252

let expandInstruction = function | Turn d -> [ Turn d ] | Move b -> [ for n in 1..b -> Move 1 ]

let visitedTwice = instructions 
                    |> Seq.collect expandInstruction 
                    |> Seq.scan move startState
                    |> Seq.find (fun f -> f.visited.Contains(f.position))
                    
printfn "Part b: Distance from home is %d" (getDistance visitedTwice.position) // my answer was 143