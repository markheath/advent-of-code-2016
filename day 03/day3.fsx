open System.Text.RegularExpressions

let isTriangle = function |[|a;b;c|] -> a+b>c && a+c>b && b+c>a | _ -> failwith "arg error"

let input = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt")

let getSides line = 
    Regex.Matches(line, "\d+") |> Seq.cast<Match> |> Seq.map (fun m -> int m.Value) |> Seq.toArray

input |> Seq.map getSides |> Seq.filter isTriangle |> Seq.length |> printfn "Part a: %d" // my answer = 982

let rotate (a:'T[][]) = 
    [| for x in {0 .. a.[0].Length-1} -> [| for y in {0 .. a.Length-1} -> a.[y].[x] |] |]

let sides = input |> Array.map getSides |> Array.chunkBySize 3 |> Seq.collect rotate 
sides |> Seq.filter isTriangle |> Seq.length |> printfn "Part b: %d" // my answer = 1826