let next = function
    | [| '^';'^';'.'|]
    | [| '.';'^';'^'|]
    | [| '^';'.';'.'|]
    | [| '.';'.';'^'|] -> '^'
    | _ -> '.'

let generateNextRow previousRow =
    ("." + previousRow + ".") 
    |> Seq.windowed 3
    |> Seq.map next
    |> Seq.toArray
    |> System.String

let assertEquals actual expected =
    if actual = expected then
        printfn "PASS %A" actual
    else 
        printfn "FAIL: got %A expected %A" actual expected

assertEquals (generateNextRow "..^^.") ".^^^^"
assertEquals (generateNextRow ".^^^^") "^^..^"

let testData = [ ".^^.^.^^^^"; "^^^...^..^"; "^.^^.^.^^.";                    "..^^...^^^"; ".^^^^.^^.^"; "^^..^.^^.."; "^^^^..^^^."; "^..^^^^.^^";                    ".^^^..^.^^"; "^^.^^^..^^" ]
let countSafe (data:seq<string>) =
    data |> Seq.collect id |> Seq.filter ((=) '.') |> Seq.length 

assertEquals (countSafe testData) 38

let generateRows startRow rows = 
    Seq.init (rows-1) id |> Seq.scan (fun s _ -> generateNextRow s) startRow 

let testRows = generateRows (testData |> List.head) 10 |> Seq.toList
for a,b in List.zip testRows testData do
    assertEquals a b 

let solve startRow rows =
     generateRows startRow rows |> countSafe


assertEquals (solve (testData |> List.head) 10) 38
let input = "^.....^.^^^^^.^..^^.^.......^^..^^^..^^^^..^.^^.^.^....^^...^^.^^.^...^^.^^^^..^^.....^.^...^.^.^^.^"

solve input 40 |> printfn "part a: %d" // 1974
let sw = System.Diagnostics.Stopwatch.StartNew()
solve input 400000 |> printfn "part b: %d" // 19991126
printfn "Part b in %A" sw.Elapsed