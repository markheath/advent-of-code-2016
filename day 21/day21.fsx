type Instruction = SwapPos of int * int
                   | SwapLetter of char * char
                   | RotateRight of int
                   | RotateLeft of int
                   | RotatePos of char
                   | Reverse of int * int
                   | Move of int * int

let swap x y (input:char[]) =
    Array.mapi (fun n c -> if n = x then input.[y] 
                            elif n = y then input.[x]
                            else c) input

swap 1 2 ("hello".ToCharArray())

let swapLetter x y =
    Array.map (fun c -> if c = x then y elif c = y then x else c)

swapLetter 'e' 'l' ("hello".ToCharArray())

let rotateRight steps (input:char[]) =
    //printfn "rotate right %d %s" steps (System.String(input))
    let s = steps % input.Length
    Array.append input.[input.Length-s..input.Length-1] input.[0..input.Length-s-1] 

rotateRight 2 ("hello".ToCharArray())
rotateRight 6 ("hello".ToCharArray())

let rotateLeft steps (input:char[]) =
    Array.append input.[steps..input.Length-1] input.[0..steps-1] 

rotateLeft 2 ("hello".ToCharArray())
rotateLeft 4 ("hello".ToCharArray())

let rotatePos c (input:char[]) =
    let n = Array.findIndex ((=) c) input
    let rot = n + (if n >= 4 then 2 else 1)
    rotateRight rot input

rotatePos 'a' ("abcde".ToCharArray()) // should rotate by 1
rotatePos 'b' ("abcde".ToCharArray()) // should rotate by 2
rotatePos 'c' ("abcde".ToCharArray()) // should rotate by 3
rotatePos 'd' ("abcde".ToCharArray()) // should rotate by 4
rotatePos 'e' ("abcde".ToCharArray()) // should rotate by 6

rotateRight 2 ("abcde".ToCharArray()) 

let reverse x y (input:char[]) =
    Array.mapi (fun n c -> if n < x || n > y then input.[n]
                           else input.[x + y - n]) input

reverse 1 3 ("habco".ToCharArray())

let moveX x y (input:char[]) =
    [|0..input.Length-1|]
    |> Array.map (fun n ->
    if (n < x && n < y) || (n > x && n > y) then
            input.[n]
        else if n = y then
            input.[x]
        else if x < y then
            input.[n+1]
        else
            input.[n-1])

let move x y (input:char[]) =
    input |> Array.mapi (fun n c ->
        if (n < x && n < y) || (n > x && n > y) then
            input.[n]
        else if n = y then
            input.[x]
        else if x < y then
            input.[n+1]
        else
            input.[n-1])


move 1 3 ("abcde".ToCharArray())
move 3 1 ("abcde".ToCharArray())
move 3 0 ("abcde".ToCharArray())
move 0 3 ("abcde".ToCharArray())
move 0 4 ("abcde".ToCharArray())
move 4 0 ("abcde".ToCharArray())
move 4 1 ("abcde".ToCharArray())

let apply = function
            | SwapPos (x,y) -> swap x y 
            | SwapLetter (x,y) -> swapLetter x y 
            | RotateRight n -> rotateRight n 
            | RotateLeft n -> rotateLeft n 
            | RotatePos c -> rotatePos c 
            | Reverse (x,y) -> reverse x y 
            | Move (x,y) -> move x y 

let parseInstruction (inst:string) =
    let parts = inst.Split(' ')
    match parts.[0..1] with
    | [| "swap"; "position" |] -> SwapPos (int parts.[2], int parts.[5])
    | [| "swap"; "letter" |] -> SwapLetter (parts.[2].[0], parts.[5].[0])
    | [| "reverse"; "positions" |] -> Reverse (int parts.[2], int parts.[4])
    | [| "rotate"; "left" |] -> RotateLeft (int parts.[2])
    | [| "rotate"; "right" |] -> RotateRight (int parts.[2])
    | [| "move"; "position" |] -> Move (int parts.[2], int parts.[5])
    | [| "rotate"; "based" |] -> RotatePos (parts.[6].[0])
    | _ -> failwith ("parse error: " + inst)

let scramble (input:string) instructions =
    instructions |> Seq.map parseInstruction |> Seq.fold (fun s i -> apply i s) (input.ToCharArray()) |> System.String

let testInstructions = [ 
                        "swap position 4 with position 0"
                        "swap letter d with letter b"
                        "reverse positions 0 through 4"
                        "rotate left 1 step"
                        "move position 1 to position 4"
                        "move position 3 to position 0"
                        "rotate based on position of letter b"
                        "rotate based on position of letter d" 
                        ]

testInstructions
|> scramble "abcde"
|> printfn "Test: %s" // decab

System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt") 
|> scramble "abcdefgh"
|> printfn "Part a: %s" // gbhafcde

let findUndo inst (st:char[]) =
    [0..st.Length-1]
    |> Seq.find (fun n -> 
                    let applied = apply (RotateLeft n) st
                    (apply inst applied) = st)

let undoRotate c (input:char[]) =
    let leftRot = findUndo (RotatePos c) input
    rotateLeft leftRot input

let undo = function
            | SwapPos (x,y) -> swap x y 
            | SwapLetter (x,y) -> swapLetter x y 
            | RotateRight n -> rotateLeft n 
            | RotateLeft n -> rotateRight n 
            | RotatePos c -> undoRotate c
            | Reverse (x,y) -> reverse x y 
            | Move (x,y) -> move y x

let roundTrip inst (s:string) =
    let applied = apply inst (s.ToCharArray())
    undo inst applied |> System.String

roundTrip (SwapPos (1,4)) "abcde" |> printfn "%s"
roundTrip (SwapLetter ('a','c')) "abcde" |> printfn "%s"
roundTrip (RotateRight 4) "abcde" |> printfn "%s"
roundTrip (RotateLeft 4) "abcde" |> printfn "%s"
roundTrip (Move (1,4)) "abcde" |> printfn "%s"
roundTrip (Reverse (2,4)) "abcde" |> printfn "%s"

let unscramble (input:string) instructions =
    instructions |> Seq.map parseInstruction |> Seq.rev |> Seq.fold (fun s i -> undo i s) (input.ToCharArray()) |> System.String

testInstructions
|> unscramble "decab"
|> printfn "Test: %s"

System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt") 
|> unscramble "fbgdceah"
|> printfn "Part b: %s" // bcfaegdh
