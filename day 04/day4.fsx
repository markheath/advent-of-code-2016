open System.Text.RegularExpressions

type Room = { name:string; sectorId:int; checksum: string}

let parseRoom room =
    Regex.Match(room,@"([a-z\-]+)(\d+)\[([a-z]+)\]").Groups
     |> Seq.cast<Group>
     |> Seq.map (fun g -> g.Value)
     |> Seq.skip 1
     |> Seq.toList
     |> function | [a;b;c] -> {name=a;sectorId=int b; checksum=c} | x -> failwith "Parse Error"

let calcChecksum room =
    room.name.Replace("-","") 
    |> Seq.countBy id 
    |> Seq.sortBy (fun (a,b)->(-b,a))
    |> Seq.map (fun (a,_) -> a.ToString())
    |> Seq.take 5
    |> System.String.Concat 


//  |> Seq.map (fst >> string)

let isRealRoom room = calcChecksum room = room.checksum

let input = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt")

let testInput = [| 
    "aaaaa-bbb-z-y-x-123[abxyz]"; 
    "a-b-c-d-e-f-g-h-987[abcde]";
    "not-a-real-room-404[oarel]";
    "totally-real-room-200[decoy]"
     |]

testInput |> Array.map parseRoom |> Array.filter isRealRoom |> Seq.sumBy (fun r -> r.sectorId) |> printfn "Test: %d" // 1514
input |> Array.map parseRoom |> Array.filter isRealRoom |> Seq.sumBy (fun r -> r.sectorId) |> printfn "Part a: %d" // 158835

