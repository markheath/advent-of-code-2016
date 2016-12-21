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

let swapLetter x y =
    Array.map (fun c -> if c = x then y elif c = y then x else c)

let rotateRight steps (input:char[]) =
    let s = steps % input.Length
    Array.append input.[input.Length-s..input.Length-1] input.[0..input.Length-s-1] 

let rotateLeft steps (input:char[]) =
    Array.append input.[steps..input.Length-1] input.[0..steps-1] 

let rotatePos c (input:char[]) =
    let n = Array.findIndex ((=) c) input
    let rot = n + (if n >= 4 then 2 else 1)
    rotateRight rot input

let reverse x y (input:char[]) =
    Array.mapi (fun n c -> if n < x || n > y then input.[n]
                           else input.[x + y - n]) input

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
let unscramble (input:string) instructions =
    instructions |> Seq.map parseInstruction |> Seq.rev |> Seq.fold (fun s i -> undo i s) (input.ToCharArray()) |> System.String

System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt") 
|> unscramble "fbgdceah"
|> printfn "Part b: %s" // bcfaegdh