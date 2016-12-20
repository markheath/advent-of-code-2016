let parseRange (r:string) = 
    let parts = r.Split('-')
    int64 parts.[0], int64 parts.[1] 

let ranges = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt") 
                |> Array.map parseRange


let getAllowed blockedRanges =
    let endRange = 4294967295L
    let rec mergeRanges n ranges = seq {
        match ranges with
        | [] -> if n <= endRange then yield (n,endRange)
        | (lo,hi)::tail ->
            //printfn "%d %d,%d" n lo hi
            if n < lo then yield (n,lo-1L)
            yield! mergeRanges (max n (hi+1L)) tail
    }
    mergeRanges 0L (blockedRanges |> Seq.sortBy fst |> Seq.toList)

getAllowed ranges |> Seq.head |> fst |> printfn "part a: %d" // 31053880

getAllowed ranges |> Seq.sumBy (fun (lo,hi) -> hi - lo + 1L) |> printfn "part b: %d" // 117