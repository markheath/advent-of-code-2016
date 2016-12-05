open System;

let md5 = System.Security.Cryptography.MD5.Create()

let findHash doorId = 
    let isInteresting (hash:byte[]) = hash.[0] = 0uy && hash.[1] = 0uy && hash.[2] <= 0xFuy
    Seq.initInfinite (sprintf "%s%d" doorId) 
    |> Seq.map (System.Text.Encoding.ASCII.GetBytes >> md5.ComputeHash)
    |> Seq.filter isInteresting
    |> Seq.map (fun h -> BitConverter.ToString(h).Replace("-",""))

let input = "abbhdwsy"

let findPassword doorId =
    findHash doorId |> Seq.take 8 |> Seq.map (fun h-> h.Substring(5,1)) |> String.Concat |> (fun s -> s.ToLower())

//findPassword "abc" |> printfn "Test: %s" // 18f47a30
let sw = System.Diagnostics.Stopwatch()
sw.Start()
findPassword input |> printfn "Part a: %s" // 801b56a7
printfn "part a in %dms" sw.ElapsedMilliseconds

let useChar (password:string) (pos, ch) =
    if pos < 8 && password.[pos] = '?' then
        sprintf "%s%c%s" (password.Substring(0,pos)) ch (password.Substring(pos+1))
    else
        password

let findPassword2 doorId =
    findHash doorId 
    |> Seq.map (fun h -> int h.[5] - int '0', h.[6]) 
    |> Seq.scan useChar "????????"
    |> Seq.find (fun f -> f.IndexOf("?") = -1)
    |> (fun s -> s.ToLower())

sw.Restart()
findPassword2 input |> printfn "Part b: %s" // 424a0197
printfn "part b in %dms" sw.ElapsedMilliseconds