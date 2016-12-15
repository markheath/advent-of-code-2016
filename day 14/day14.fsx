open System.Text.RegularExpressions
open System
open System.Text
let md5 = System.Security.Cryptography.MD5.Create()

// only needed to consider the first match
let threeInARow s = s
                        |> Seq.windowed 3
                        |> Seq.tryFind (function | [|a;b;c|] -> a=b && b=c | _ -> false)
                        |> Option.map (fun a -> a.[0])

//threeInARow "Helllo Wooorld"

let hash (s:string) =
    Encoding.Default.GetBytes(s)
    |> md5.ComputeHash
    |> (fun h -> BitConverter.ToString(h).ToLower().Replace("-",""))

let getHash salt n =
    sprintf "%s%d" salt n
    |> hash

getHash "abc" 18 |> threeInARow // 8
getHash "abc" 39 |> threeInARow // e


let stretchedHash salt n =
    [1..2016] |> List.fold (fun s n -> hash s) (getHash salt n)
stretchedHash "abc" 0 // a107ff634856bb300138cac6568c0f24
let isKey (hashes:string[]) =
    let next1000Contains rpt =
        let find = String(rpt,5)
        [for n in 1..1000 -> hashes.[n]] 
        |> Seq.exists (fun h -> h.Contains(find))

    match threeInARow hashes.[0] with
    | Some c -> next1000Contains c
    | _ -> false
let solve hasher targetIndex =
    Seq.initInfinite id
    |> Seq.map hasher 
    |> Seq.windowed 1001
    |> Seq.indexed
    |> Seq.filter (snd >> isKey)
    |> Seq.skip (targetIndex-1)
    |> Seq.head
    |> fst

solve (getHash "abc") 64 |> printfn "test: %d" // 22728 

let input = "ngcjuoqr"
solve (getHash input) 64 |> printfn "part a: %d"  // 18626
solve (stretchedHash input) 64 |> printfn "part b: %d" // 20092

