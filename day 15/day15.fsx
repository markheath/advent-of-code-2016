let isOpenAtT (positions,startPos) time =
    ((startPos + time) % positions) = 0

let isSolution disks startTime =
    disks
    |> Seq.indexed
    |> Seq.forall (fun (n,disk) -> isOpenAtT disk (startTime+1+n))

let solve disks =
    Seq.initInfinite id
    |> Seq.find (isSolution disks)

solve [(5,4);(2,1)] |> printfn "Test: %d" // should be 5 

open System.Text.RegularExpressions

let parseInput input = 
    let parts = Regex.Matches(input,@"\d+")
                        |> Seq.cast<Match>
                        |> Seq.map (fun m-> int m.Value)
                        |> Seq.toList
    match parts with | [_;pos;_;start] -> (pos,start) | _ -> failwith ("parse error" + input)


let discs = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt") |> Array.map parseInput

solve discs |> printfn "Part a: %d" // 16824

solve (Seq.append discs [(11,0)]) |> printfn "Part b: %d"