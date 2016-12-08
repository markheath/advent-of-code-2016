open System.Text.RegularExpressions;
let input = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt")

type Instruction = Rect of int * int | RotateRow of int * int | RotateCol of int * int 
let (|Matches|) pattern =
  let rgx = Regex (pattern, RegexOptions.Compiled)
  fun input -> [for m in rgx.Matches input -> [for g in m.Groups -> g.Value]]

let (|Int|_|) str = match System.Int32.TryParse str with true, value -> Some value | _ -> None

let parseInstruction = function 
    | Matches "rect (\d+)x(\d+)" [[_; Int width; Int height ]] -> Rect (width, height)
    | Matches "rotate row y=(\d+) by (\d+)" [[_; Int r; Int n ]] -> RotateRow (r, n)
    | Matches "rotate column x=(\d+) by (\d+)" [[_; Int r; Int n ]] -> RotateCol (r, n)
    | u -> failwith ("oops " + u) 


let rect (width,height) (state:int[,]) =
    let changed = Array2D.copy state
    for x in 0..width-1 do
        for y in 0..height-1 do
            changed.[y,x] <- 1
    changed

let rotateRow (row,n) (state:int[,]) =
    let changed = Array2D.copy state
    let rowWidth = Array2D.length2 state
    for x in 0..rowWidth-1 do
        changed.[row, ((x+n)%rowWidth)] <- state.[row,x]
    changed

let rotateCol (col,n) (state:int[,]) =
    let changed = Array2D.copy state
    let colHeight = Array2D.length1 state
    for y in 0..colHeight-1 do
        changed.[((y+n)%colHeight),col] <- state.[y,col]
    changed

let testState = Array2D.create 3 7 0

rect (3,2) testState |> rotateCol (1,1) |> rotateRow (0,4) |> rotateCol (1,1)
let startState = Array2D.create 6 50 0

let apply = function | Rect (a,b) -> rect (a,b) | RotateCol (a,b) -> rotateCol (a,b) | RotateRow (a,b) -> rotateRow (a,b)

input 
    |> Array.map parseInstruction 
    |> Array.fold apply startState 

