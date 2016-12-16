open System.Text
let append (sb:StringBuilder) (c:char) = sb.Append (c) |> ignore

let rec expandOnce (input:string) =
    let len = input.Length
    let sb = StringBuilder(input, 1 + len * 2)
    append sb '0'
    for n in 1..len do 
        append sb (if input.[len-n] = '1' then '0' else '1')
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
    let csLen = input.Length / 2
    let sb = StringBuilder(csLen)
    for n in 0..csLen-1 do
        append sb (if input.[n*2] = input.[n*2+1] then '1' else '0')
    sb.ToString()        

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
let sw = System.Diagnostics.Stopwatch.StartNew()
solve "11110010111001001" 272 |> printfn "part a: %s" // 01110011101111011
printfn "%A" sw.Elapsed
sw.Restart()
solve "11110010111001001" 35651584 |> printfn "part b: %s" // 11001111011000111
printfn "%A" sw.Elapsed