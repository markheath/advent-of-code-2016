
let input = System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + "\\input.txt")
open System.Text.RegularExpressions

let (|Matches|) pattern =
  let rgx = Regex (pattern, RegexOptions.Compiled)
  fun input -> [for m in rgx.Matches input do for g in m.Groups -> g.Value]

let (|Int|_|) str = match System.Int32.TryParse str with true, value -> Some value | _ -> None

type ExpandState = { parsePos: int; decoded: string }

let decompress (input:string) =
    let rec expand state =
        let m = Regex.Match(input.Substring(state.parsePos), @"^\((\d+)x(\d+)\)")
        if state.parsePos >= input.Length then 
            state.decoded
        else
            if m.Success then
                let chars = int m.Groups.[1].Value
                let repeats = int m.Groups.[2].Value
                let repeat = input.Substring(state.parsePos + m.Index + m.Length, chars)
                let expanded = [for i in 1..repeats -> repeat] |> List.fold (+) ""
                expand { parsePos = state.parsePos+m.Length+chars;
                         decoded = state.decoded + expanded}
            else
                expand { parsePos = state.parsePos+1; decoded = state.decoded + input.Substring(state.parsePos,1) }
    
    expand { parsePos = 0; decoded = ""}
    
decompress "ADVENT"
decompress "A(1x5)BC"
decompress "(3x3)XYZ"
decompress "(6x1)(1x3)A"
decompress "X(8x2)(3x3)ABCY"

(decompress input).Length |> printfn "Part a: %d"