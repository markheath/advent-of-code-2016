open System.Text.RegularExpressions
let isPossibleTriangle sides =
    let sorted = sides |> Array.sort
    sorted.[0] + sorted.[1] > sorted.[2]

let input = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt")

let getSides line = 
    Regex.Matches(line, "\d+") |> Seq.cast<Match> |> Seq.map (fun m -> int m.Value) |> Seq.toArray

input |> Seq.map getSides |> Seq.filter isPossibleTriangle |> Seq.length |> printfn "Part a: %d" // my answer = 982

let batch batchSize (array:'T[]) =
    [for b in 0 .. (array.Length / batchSize) - 1 -> b * batchSize]
    |> Seq.map (fun n -> array.[n..n+batchSize-1])  

let rotate (a:'T[][]) = 
    [| for x in [0..a.[0].Length-1] -> [| for y in [0..a.Length-1] -> a.[y].[x] |]  |]

let sides = input |> Array.map getSides |> batch 3 |> Seq.map rotate |> Seq.collect id 
sides |> Seq.filter isPossibleTriangle |> Seq.length |> printfn "Part b: %d" // my answer = 1826
