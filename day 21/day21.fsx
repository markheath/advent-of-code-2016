type Instruction = SwapPos of int * int
                   | SwapLetter of char * char
                   | RotateRight of int
                   | RotateLeft of int
                   | RotatePos of char
                   | Reverse of int * int
                   | Move of int * int

let swap x y (a:_[]) = Array.mapi (fun n c -> if n = x then a.[y] elif n = y then a.[x] else c) a

let swapLetter x y = Array.map (fun c -> if c = x then y elif c = y then x else c)

let rotateLeft n (a:_[]) = [|for i in 0..a.Length-1 -> a.[(n+i)%a.Length]|]
let rotateRight n (a:_[]) = rotateLeft (a.Length-(n%a.Length)) a

let rotatePos c (a:_[]) =
    let n = Array.findIndex ((=) c) a
    rotateRight (n + if n >= 4 then 2 else 1) a

let reverse x y (a:_[]) =
    Array.mapi (fun n c -> if n < x || n > y then a.[n] else a.[x + y - n]) a

let move x y (a:_[]) =
    a |> Array.mapi (fun n c ->
        if (n < x && n < y) || (n > x && n > y) then
            a.[n]
        elif n = y then
            a.[x]
        elif x < y then
            a.[n+1]
        else
            a.[n-1])

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

let undoRotate c (a:_[]) =
    a |> Array.mapi (fun n c -> n, apply (RotateLeft n) a) |> Seq.find (fun (n,t) -> (apply (RotatePos c) t) = a) |> snd

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