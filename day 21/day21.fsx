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
    Array.append input.[input.Length-steps..input.Length-1] input.[0..input.Length-steps-1] 

rotateRight 2 ("hello".ToCharArray())
// might need bounds check: rotateRight 6 ("hello".ToCharArray())

let rotateLeft steps (input:char[]) =
    Array.append input.[steps..input.Length-1] input.[0..steps-1] 

rotateLeft 2 ("hello".ToCharArray())

let rotatePos c (input:char[]) =
    let n = Array.findIndex ((=) c) input
    let rot = n + (if n >= 4 then 2 else 1)
    rotateRight rot input

rotatePos 'e' ("hello".ToCharArray()) // should rotate by 2
rotatePos 'o' ("hello world".ToCharArray()) // should rotate by 6

let reverse x y (input:char[]) =
    Array.mapi (fun n c -> if n < x || n > y then input.[n]
                           else input.[x + y - n]) input

reverse 1 3 ("habco".ToCharArray())

let move x y (input:char[]) =
    

let scramble input =
