open System
let testInput = "ULL  
RRDDD
LURDL
UUUUD"
let input = System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + "\\input.txt")

let keypad =  Array2D.init 3 3 (fun x y -> (x*3 + y + 1))
let instructions = input.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)

let followInstruction pos instruction = 
    let addv (x,y) (i,j) = x+i,y+j
    let move = match instruction with | 'U' -> (0, -1) | 'D' -> (0, 1) | 'R' -> (1,0) | 'L' -> (-1,0) | _ -> (0,0)
    let x', y' = addv pos move
    let constrain n l u = max (min n u) l 
    (constrain x' 0 2), (constrain y' 0 2)

//followInstruction (1,0) 'U'
followInstruction (1,2) 'L'

let followLine pos line =
    line |> Seq.fold followInstruction pos

let folder (pos,nums) line =
    let x,y = followLine pos line
    (x,y), (keypad.[y,x])::nums

//instructions |> Seq.fold folder ((1,1),[])
let lookup (x,y) = keypad.[y,x]

instructions |> Seq.scan followLine (1,1) |> Seq.skip 1 |> Seq.map lookup


let startPos = 1,1
