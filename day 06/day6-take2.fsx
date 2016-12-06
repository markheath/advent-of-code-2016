let zip (a:string[]) =
    [| for x in 0..a.[0].Length-1 -> [| for y in a -> y.[x] |] |]
let decodeMessages selector = 
    let decodeChar = Seq.countBy id >> selector snd >> fst
    zip >> (Array.map decodeChar) >> System.String

//let decodeMessages selector =
//    zip >> (Array.map (Seq.countBy id >> selector snd >> fst)) >> System.String

let input = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt")
decodeMessages Seq.maxBy input |> printfn "Part a: %s" // kjxfwkdh
decodeMessages Seq.minBy input |> printfn "Part b: %s" // xrwcsnps
