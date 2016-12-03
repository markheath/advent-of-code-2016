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
    |> Seq.map (fun n -> [| for i in [n..n+batchSize-1] -> array.[i]|])  

let swap3 [| [|a1;b1;c1;|] ; [|a2;b2;c2;|] ; [|a3;b3;c3;|] |] =
            [| [|a1;a2;a3;|]; [|b1;b2;b3;|]; [|c1;c2;c3;|] |]

let sides = input |> Array.map getSides |> batch 3 |> Seq.map swap3 |> Seq.collect id 
sides |> Seq.filter isPossibleTriangle |> Seq.length |> printfn "Part b: %d" // my answer = 1826
