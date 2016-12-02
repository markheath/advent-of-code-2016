let solve (keypad:string[]) startPos input =
    let lookup (x,y) = keypad.[y].[x]
    let isValid pos = lookup pos <> ' '
    let followInstruction pos instruction = 
        let addv (x,y) (i,j) = x+i,y+j
        let move = match instruction with | 'U' -> (0,-1) | 'D' -> (0,1) | 'R' -> (1,0) | 'L' -> (-1,0) | _ -> (0,0)
        let newPos = addv pos move
        if isValid newPos then newPos else pos
    let followLine = Seq.fold followInstruction
    input 
        |> Seq.scan followLine startPos
        |> Seq.skip 1 
        |> Seq.map (lookup >> string) 
        |> System.String.Concat 
        |> printfn "Code: %s"

let keypadA = [| "     "; " 123 "; " 456 "; " 789 "; "     " |]
let keypadB = [| "       "; "   1   "; "  234  "; " 56789 "; "  ABC  "; "   D   "; "       " |]

let testInput = [| "ULL  "; "RRDDD"; "LURDL";  "UUUUD" |]
let input = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt")

solve keypadA (2,2) testInput
solve keypadA (2,2) input // part a - my answer is 44558
solve keypadB (1,3) input // part b - my answer is 6BBAD