let testInput = [| 
                "eedadn"
                "drvtee"
                "eandsr"
                "raavrd"
                "atevrs"
                "tsrnev"
                "sdttsa"
                "rasrtv"
                "nssdts"
                "ntnada"
                "svetve"
                "tesnvt"
                "vntsnd"
                "vrdear"
                "dvrsen"
                "enarar"
                |]

let mostFrequentAtPos (messages:seq<string>) n =
    messages |> Seq.map (fun msg -> msg.[n]) |> Seq.countBy id |> Seq.maxBy snd |> fst



[|0..5|] |> Array.map (mostFrequentAtPos testInput) |> System.String |> printfn "Test: %s"
//mostFrequentAtPos 0
let input = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt")

[|0..7|] |> Array.map (mostFrequentAtPos input) |> System.String |> printfn "Part a: %s" // kjxfwkdh

let leastFrequentAtPos (messages:seq<string>) n =
    messages |> Seq.map (fun msg -> msg.[n]) |> Seq.countBy id |> Seq.minBy snd |> fst
[|0..5|] |> Array.map (leastFrequentAtPos testInput) |> System.String |> printfn "Test: %s"
[|0..7|] |> Array.map (leastFrequentAtPos input) |> System.String |> printfn "Part b: %s" // xrwcsnps
