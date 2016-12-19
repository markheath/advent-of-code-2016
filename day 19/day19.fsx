type State = { ElfNumber:int; ElfOnLeft:int} 

let generateStartState elves = 
    Seq.init elves (fun n ->   n+1, 
                               (n+1)%elves+1 )
    |> Map.ofSeq

let rec solve (state:Map<int,int>) pos =
    let left = state.[pos]
    //printfn "Elf %d takes from %d: %A" pos left state
    if left = pos then 
        pos
    else
        solve (state.Add(pos, state.[left]).Remove(left)) state.[left]

generateStartState 5 |> printfn "%A"
solve (generateStartState 5) 1 |> printfn "Test: %d" // 3

solve (generateStartState 3014387) 1 |> printfn "Part a: %d" // 1834471
