let input = System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + "\\input.txt")
open System.Text.RegularExpressions

type ExpandState = { parsePos: int; decoded: string }

let decompress (input:string) =
    let rec expand state =
        let sub = input.Substring(state.parsePos)
        let m = Regex.Match(sub, @"\((\d+)x(\d+)\)")
        if m.Success then
            let chars = int m.Groups.[1].Value
            let repeats = int m.Groups.[2].Value
            let repeat = input.Substring(state.parsePos + m.Index + m.Length, chars)
            let expanded = [for i in 1..repeats -> repeat] |> List.fold (+) ""
            expand { parsePos = state.parsePos+m.Index+m.Length+chars;
                        decoded = state.decoded + sub.Substring(0,m.Index) + expanded}
        else
            state.decoded + sub
    
    expand { parsePos = 0; decoded = ""}
    
decompress "ADVENT" |> printfn "%s" // ADVENT
decompress "A(1x5)BC"  |> printfn "%s" // ABBBBBC
decompress "(3x3)XYZ"  |> printfn "%s" // XYZXYZXYZ
decompress "(6x1)(1x3)A"  |> printfn "%s" // (1x3)A
decompress "X(8x2)(3x3)ABCY"  |> printfn "%s" // X(3x3)ABC(3x3)ABCY

(decompress input).Length |> printfn "Part a: %d" // 110346


type ExpandState2 = { parsePos: int; decodedLen: int64 }

let rec decompressLen (input:string) =
    let rec expand state =
        let m = Regex.Match(input.Substring(state.parsePos), @"\((\d+)x(\d+)\)")
        if m.Success then
            let chars = int m.Groups.[1].Value
            let repeats = int m.Groups.[2].Value
            let repeat = input.Substring(state.parsePos + m.Index + m.Length, chars)
            let expandedLen = (decompressLen repeat) * int64 repeats 
            expand { parsePos = state.parsePos + m.Index + m.Length + chars;
                        decodedLen = state.decodedLen + int64 m.Index + expandedLen}
        else
            state.decodedLen + int64(input.Length-state.parsePos)
    
    expand { parsePos = 0; decodedLen = 0L}

decompressLen "(27x12)(20x12)(13x14)(7x10)(1x12)A" |> printfn "Test: %d" // 241920
decompressLen "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" |> printfn "Test: %d"  // 445
decompressLen input |> printfn "Part b: %d" // 10774309173