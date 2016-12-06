
let decodePos (messages:seq<string>) selector n =
    messages |> Seq.map (fun msg -> msg.[n]) |> Seq.countBy id |> selector snd |> fst
let decodeMessages (messages:string[]) selector =
    [|0..messages.[0].Length-1|] |> Array.map (decodePos messages selector) |> System.String 

let input = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt")
decodeMessages input Seq.maxBy |> printfn "Part a: %s" // kjxfwkdh
decodeMessages input Seq.minBy |> printfn "Part b: %s" // xrwcsnps
