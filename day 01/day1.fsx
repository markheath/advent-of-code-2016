open System
type Turn = Right | Left
type Instruction = Turn of direction : Turn | Move
type State = { position: int * int; facing: int*int; visited: Set<int*int> }
let input = System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + "\\input.txt")
let parseInstruction (i:string) = seq {
    yield match i.[0] with | 'L' -> Turn Left | 'R' -> Turn Right | _ -> failwith "parseError"
    yield! [for n in 1..int(i.Substring(1)) -> Move]
}
let instructions = input.Split([|", "|], StringSplitOptions.None) |> Seq.collect parseInstruction

let rotate (x,y) = function | Right -> (y*(-1),x) | Left -> (y,x*(-1)) 

let move state instruction =
    match instruction with 
    | Turn direction ->
        { state with facing = rotate state.facing direction }
    | Move ->
        let addv (x,y) (i,j) = (x+i,y+j)
        { state with position = addv state.position state.facing; visited = Set.add state.position state.visited  }
       
let startState = { position = (0,0); facing = (0,-1); visited = Set.empty } 

let endState = instructions |> Seq.fold move startState

let getDistance (x,y) = abs(x) + abs(y)

printfn "Part a: Distance from home is %d" (getDistance endState.position) // my answer was 252

let visitedTwice = instructions 
                    |> Seq.scan move startState
                    |> Seq.find (fun f -> f.visited.Contains(f.position))
                    
printfn "Part b: Distance from home is %d" (getDistance visitedTwice.position) // my answer was 143