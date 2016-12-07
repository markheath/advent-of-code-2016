open System.Text.RegularExpressions;
let input = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt")


let supportsTls ipAddress = 
    let parts = Regex.Split(ipAddress,@"\[(.*?)\]")
    let supernetSequences = [|for n in 0..parts.Length-1 do if n % 2 = 0 then yield parts.[n] |] // outside square brackets
    let inBrackets = [|for n in 0..parts.Length-1 do if n % 2 = 1 then yield parts.[n] |]
    //let containsAbba s = Regex.IsMatch(s, @"(\w)(\w)\2\1")
    let containsAbba s = s |> Seq.windowed 4 |> Seq.exists (function | [|a;b;c;d|] -> a=d&&b=c&&a<>b | _ -> false)
    (supernetSequences |> Array.exists containsAbba) && not (inBrackets |> Array.exists containsAbba)
    //parts |> Array.mapi (fun n a -> n % 2 = 1) |> Array.exists containsAbba

supportsTls "abba[mnop]qrst" // true
supportsTls "abcd[bddb]xyyx" // false
supportsTls "aaaa[qwer]tyui" // false
supportsTls "ioxxoj[asdfgh]zxcvbn" // true

input |> Seq.filter supportsTls |> Seq.length |> printfn "Part a: %d"

let supportsSsl ipAddress = 
    let parts = Regex.Split(ipAddress,@"\[(.*?)\]")
    let supernetSequences = [|for n in 0..parts.Length-1 do if n % 2 = 0 then yield parts.[n] |] // outside square brackets
    let hypernetSequences = [|for n in 0..parts.Length-1 do if n % 2 = 1 then yield parts.[n] |]
    let findAbas s = s |> Seq.windowed 3 |> Seq.filter (function | [|a;b;c|] -> a=c&&a<>b | _ -> false) |> Seq.map System.String
    let abas = supernetSequences |> Seq.collect findAbas
    let makeBab (aba:string) = sprintf "%c%c%c" aba.[1] aba.[0] aba.[1]
    let babExists bab = hypernetSequences |> Seq.exists (fun s -> s.Contains(bab))
    supernetSequences |> Seq.collect findAbas |> Seq.exists (makeBab >> babExists)

supportsSsl "aba[bab]xyz" // true 
supportsSsl "xyx[xyx]xyx" // false 
supportsSsl "aaa[kek]eke" // true
supportsSsl "zazbz[bzb]cdb" // true

input |> Seq.filter supportsSsl |> Seq.length |> printfn "Part b: %d"
