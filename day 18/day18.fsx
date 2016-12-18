let isSafe x =
    match x with
    | [| '^';'^';'.'|]
    | [| '.';'^';'^'|]
    | [| '^';'.';'.'|]
    | [| '.';'.';'^'|] -> false
    | _ -> true

let generateNextRow (previousRow:string) =
    ("." + previousRow + ".") 
    |> Seq.windowed 3
    |> Seq.map (fun x -> if isSafe x then '.' else '^')
    |> Seq.toArray
    |> System.String

let assertEquals actual expected =
    if actual = expected then
        printfn "PASS %A" actual
    else 
        printfn "FAIL: got %A expected %A" actual expected

assertEquals (generateNextRow "..^^.") ".^^^^"
assertEquals (generateNextRow ".^^^^") "^^..^"


let testData = [
                    ".^^.^.^^^^"
                    "^^^...^..^"
                    "^.^^.^.^^."
                    "..^^...^^^"
                    ".^^^^.^^.^"
                    "^^..^.^^.."
                    "^^^^..^^^."
                    "^..^^^^.^^"
                    ".^^^..^.^^"
                    "^^.^^^..^^"
            ]
let count (data:seq<string>) =
    data 
    |> Seq.map (fun s -> s |> Seq.filter ((=) '.') |> Seq.length)  |> Seq.sum  

assertEquals (count testData) 38

let generateRows (startRow:string) (rows:int) = 
    Seq.append [startRow]
                (Seq.unfold (fun state -> 
                    let next =generateNextRow state 
                    Some (next, next)) startRow)
    
    |> Seq.take rows
    
 
let rec generateRows2 (startRow:string) rows = 
    seq {
        if rows > 0 then
            yield startRow
            yield! generateRows2 (generateNextRow startRow) (rows-1)
    }

let testRows = generateRows2 (testData |> List.head) 10 |> Seq.toList
for a,b in List.zip testRows testData do
    assertEquals a b 

let solveA startRow rows =
     generateRows2 startRow rows 
     |> Seq.collect id
     |> Seq.filter ((=) '.')
     |> Seq.length

assertEquals (solveA (testData |> List.head) 10) 38
let input = "^.....^.^^^^^.^..^^.^.......^^..^^^..^^^^..^.^^.^.^....^^...^^.^^.^...^^.^^^^..^^.....^.^...^.^.^^.^"

solveA input 40 |> printfn "part a: %d" // 1974
let sw = System.Diagnostics.Stopwatch.StartNew()
solveA input 400000 |> printfn "part b: %d" // 19991126
printfn "Part b in %A" sw.Elapsed