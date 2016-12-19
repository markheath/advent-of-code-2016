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

let solveB elves =

    let rec solveB2 (state:int[]) len pos =
        let elf = state.[pos]
        //printfn "Elf %d's turn %A" elf state
        if len % 1000 = 0 then
            printfn "%d" len

        if len = 1 then 
            elf
        else
            let oppositeIndex = (pos + len / 2) % len
            let eliminateElf = state.[oppositeIndex]
            let newLen = len-1
            for n in oppositeIndex..newLen-1 do state.[n] <- state.[n+1]
            
            //let newState = Array.filter (fun e -> e <> eliminateElf) state
            //printfn "Elf %d takes from %d" elf eliminateElf
            let leftPos = if oppositeIndex > pos then (pos+1)% (newLen) else pos% (newLen)
            solveB2 state newLen leftPos
    solveB2 (Array.init elves (fun n -> n+1)) elves 0


solveB 5 |> printfn "Test b: %d" // 2
solveB 3014387 |> printfn "Part b: %d" // 1420064