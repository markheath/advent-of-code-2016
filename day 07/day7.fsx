open System.Text.RegularExpressions;
let input = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt")

let filterByIndex predicate sequence = 
    sequence |> Seq.indexed |> Seq.filter (fst >> predicate) |> Seq.map snd 

let parseIpAddress ipAddress =
    let parts = Regex.Split(ipAddress,@"\[(.*?)\]")
    let supernetSequences = parts |> filterByIndex (fun n -> n % 2= 0) |> Seq.toArray 
    let hypernetSequences = parts |> filterByIndex (fun n -> n % 2= 1) |> Seq.toArray
    supernetSequences, hypernetSequences

let supportsTls ipAddress = 
    let super,hyper = parseIpAddress ipAddress
    let containsAbba s = s |> Seq.windowed 4 |> Seq.exists (function | [|a;b;c;d|] -> a=d&&b=c&&a<>b | _ -> false)
    (super |> Array.exists containsAbba) && not (hyper |> Array.exists containsAbba)

input |> Seq.filter supportsTls |> Seq.length |> printfn "Part a: %d" // 105

let supportsSsl ipAddress = 
    let super,hyper = parseIpAddress ipAddress
    let findAbas s = s |> Seq.windowed 3 |> Seq.filter (function | [|a;b;c|] -> a=c&&a<>b | _ -> false) |> Seq.map System.String
    let abas = super |> Seq.collect findAbas
    let makeBab (aba:string) = sprintf "%c%c%c" aba.[1] aba.[0] aba.[1]
    let babExists bab = hyper |> Seq.exists (fun s -> s.Contains(bab))
    super |> Seq.collect findAbas |> Seq.exists (makeBab >> babExists)

input |> Seq.filter supportsSsl |> Seq.length |> printfn "Part b: %d" // 258
