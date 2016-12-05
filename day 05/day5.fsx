open System;

let md5 = System.Security.Cryptography.MD5.Create()
let findHash doorId = seq {
    let prefix = "00000"
    for n in 0 .. 100000000 do
    let inputString = sprintf "%s%d" doorId n
    let inputBytes = System.Text.Encoding.ASCII.GetBytes(inputString)
    let hashBytes = md5.ComputeHash(inputBytes)
    let hashString = BitConverter.ToString(hashBytes).Replace("-","")
    if hashString.StartsWith(prefix) then yield hashString
}
let input = "abbhdwsy"

let findPassword doorId =
    findHash doorId |> Seq.take 8 |> Seq.map (fun h-> h.Substring(5,1)) |> String.Concat |> (fun s -> s.ToLower())

//findPassword "abc" |> printfn "Test: %s" // 18f47a30

findPassword input |> printfn "Part a: %s" // 801b56a7

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

findPassword2 input |> printfn "Part b: %s" // 424a0197
