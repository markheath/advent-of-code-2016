open System.Text.RegularExpressions
type Turn = Right | Left
type Instruction = Turn of Turn | Move
type State = { pos: int*int; facing: int*int; visited: Set<int*int> }
let startState = { pos = (0,0); facing = (0,-1); visited = Set.empty } 
let rotate (x,y) = function | Right -> (y*(-1),x) | Left -> (y,x*(-1)) 
let getDistance (x,y) = abs(x) + abs(y)
let input = System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + "\\input.txt")
let instructions = Regex.Matches(input, "([LR])|(\\d+)") 
                    |> Seq.cast<Match>
                    |> Seq.map (fun m -> m.Value)
                    |> Seq.collect (function | "L" -> [Turn Left] | "R" -> [Turn Right] | n -> [for _ in 1..int(n) -> Move])

let move state instruction =
    match instruction with 
    | Turn direction ->
        { state with facing = rotate state.facing direction }
    | Move ->
        let addv (x,y) (i,j) = (x+i,y+j)
        { state with pos = addv state.pos state.facing; visited = Set.add state.pos state.visited  }
       
let endState = instructions |> Seq.fold move startState

printfn "Part a: Distance from home is %d" (getDistance endState.pos) // my answer was 252

let visitedTwice = instructions 
                    |> Seq.scan move startState
                    |> Seq.find (fun f -> f.visited.Contains(f.pos))
                    
printfn "Part b: Distance from home is %d" (getDistance visitedTwice.pos) // my answer was 143