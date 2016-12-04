open System.Text.RegularExpressions

type Room = { name:string; sectorId:int; checksum: string}

let parseRoom room =
    Regex.Match(room,@"([a-z\-]+)(\d+)\[([a-z]+)\]").Groups
     |> Seq.cast<Group>
     |> Seq.map (fun g -> g.Value)
     |> Seq.toList
     |> function | [_;a;b;c] -> {name=a;sectorId=int b; checksum=c} | x -> failwith "Parse Error"

let calcChecksum (roomName:string) =
    roomName.Replace("-","") 
    |> Seq.countBy id 
    |> Seq.sortBy (fun (a,b)->(-b,a))
    |> Seq.map (fst >> string)
    |> Seq.take 5
    |> System.String.Concat 

let isRealRoom room = calcChecksum room.name = room.checksum

let input = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt")

let testInput = [| 
    "aaaaa-bbb-z-y-x-123[abxyz]"; 
    "a-b-c-d-e-f-g-h-987[abcde]";
    "not-a-real-room-404[oarel]";
    "totally-real-room-200[decoy]"
     |]

testInput |> Array.map parseRoom |> Array.filter isRealRoom |> Seq.sumBy (fun r -> r.sectorId) |> printfn "Test: %d" // 1514
input |> Array.map parseRoom |> Array.filter isRealRoom |> Seq.sumBy (fun r -> r.sectorId) |> printfn "Part a: %d" // 158835

let shiftBy (n:int) (c:char) =
    char (int 'a' + (int c - int 'a' + n) % 26)

let decryptName n name =
    name |> Seq.map (function | '-' -> ' ' | a -> shiftBy n a) |> System.String.Concat

let decryptRoom r = decryptName r.sectorId r.name

decryptName 343 "qzmt-zixmtkozy-ivhz" |> printfn "Test: %s"

let isStorageRoom r = (decryptRoom r).StartsWith("northpole object storage")

let storageRoom = input |> Array.map parseRoom |> Array.filter isRealRoom |> Array.find isStorageRoom
printfn "Part b: %d" storageRoom.sectorId // 993 