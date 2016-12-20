let parseRange (r:string) = 
    let parts = r.Split('-')
    int64 parts.[0], int64 parts.[1] 

let solve ranges =
    ranges 
    |> Seq.sortBy fst
    |> Seq.fold (fun lowest (low,high) -> if lowest < low then lowest else if lowest > high then lowest else high + 1L) 0L

let ranges = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt") 
                |> Array.map parseRange

solve [(5L,8L);(0L,2L);(4L,7L)]
solve ranges |> printfn "Part a: %d" // 31053880

let sRanges = ranges |> Array.sortBy fst

let rec mergeRanges n ranges = seq {
    match ranges with
    | [] -> yield (n,4294967295L)
    | head::tail ->
        let lo,hi = head
        //printfn "%d %d,%d" n lo hi
        if n < lo then
            yield (n,lo-1L)
        yield! mergeRanges (max n (hi+1L)) tail
}

mergeRanges 0L ([(5L,8L);(0L,2L);(4L,7L); (11L,13L)] |> List.sortBy fst) 

let allowed = sRanges |> Array.toList |> mergeRanges 0L |> Seq.toList

allowed |> Seq.sumBy (fun (lo,hi) -> hi - lo + 1L) |> printfn "part b: %d" // 117