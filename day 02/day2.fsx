let testInput = [| "ULL  "; "RRDDD"; "LURDL";  "UUUUD" |]
let input = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt")

let keypadA = [| "     "; " 123 "; " 456 "; " 789 "; "     " |]
let keypadB = [| "       "; "   1   "; "  234  "; " 56789 "; "  ABC  "; "   D   "; "       " |]

let keypad = keypadB // keypadA
let startPos = (3,3) //  (2,2)          

let lookup (x,y) = keypad.[y].[x]

let followInstruction pos instruction = 
    let addv (x,y) (i,j) = x+i,y+j
    let move = match instruction with | 'U' -> (0, -1) | 'D' -> (0, 1) | 'R' -> (1,0) | 'L' -> (-1,0) | _ -> (0,0)
    let newPos = addv pos move
    if lookup newPos = ' ' then pos else newPos

let followLine pos line =
    line |> Seq.fold followInstruction pos

// part a - my answer is 44558, part b - my answer is 6BBAD
input 
    |> Seq.scan followLine startPos
    |> Seq.skip 1 
    |> Seq.map (lookup >> string) 
    |> System.String.Concat 
    |> printfn "Part a: %s"
