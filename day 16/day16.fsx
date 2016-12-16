let rec expandOnce (input:string) =
    let len = input.Length
    let sb = System.Text.StringBuilder(input, 1 + len * 2)
    sb.Append('0') |> ignore
    let flip c = if c = '1' then '0' else '1'
    [ for n in 1..len -> flip input.[len-n] ]
        |> Seq.iter (sb.Append >> ignore)
    sb.ToString()

expandOnce "1" |> printfn "Test: %s (expect 100)"
expandOnce "0" |> printfn "Test: %s (expect 001)"
expandOnce "11111" |> printfn "Test: %s (expect 11111000000)"
expandOnce "111100001010" |> printfn "Test: %s (expect 1111000010100101011110000)"

let rec expand (initial:string) targetSize =
    if initial.Length >= targetSize then
        initial.Substring(0,targetSize)
    else
        expand (expandOnce initial) targetSize

let checkSumOnce (input:string) =
    // assume even length input
    input 
        |> Seq.chunkBySize 2 
        |> Seq.map (fun [|a;b|] -> if a = b then '1' else '0')
        |> Seq.toArray
        |> System.String

checkSumOnce "110010110100" // 110101    
checkSumOnce "110101" // 100 

let rec checkSum input =
    let cs = checkSumOnce input
    if cs.Length % 2 = 1 then cs else checkSum cs

checkSumOnce "110010110100" // 100    
            
let solve initialState diskSize =
    let expanded = expand initialState diskSize
    checkSum expanded

solve "10000" 20 // 01100
solve "11110010111001001" 272 |> printfn "part a: %s" // 01110011101111011
printfn "%A" DateTime.UtcNow
solve "11110010111001001" 35651584 |> printfn "part b: %s"