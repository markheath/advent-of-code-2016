// breadth first solver, optimized enough to get part b solved in <20 mins. Lots more scope for speedup!
open System.Collections.Generic;
type Item = Gen of string | Chip of string

type State = { floor: int; items: Map<Item, int>  }

let rec combinations acc size set = seq {
  match size, set with 
  | n, x::xs -> 
      if n > 0 then yield! combinations (x::acc) (n - 1) xs
      if n >= 0 then yield! combinations acc n xs 
  | 0, [] -> yield acc 
  | _, [] -> () }

let comb size set = combinations[] size set

let isValid items =
    let hasGenerator t = List.contains (Gen t) items
    let noGenerators = items |> List.forall (function | Chip _ -> true | _ -> false)
    let allMicrochipsMatch = items |> List.forall (function | Chip m -> hasGenerator m | _ -> true) 
    noGenerators || allMicrochipsMatch

let isSolution state = state.items |> Map.forall (fun _ floor -> floor = 4)

let itemsOnFloor n state = state.items |> Map.toSeq |> Seq.filter (fun (item,floor) -> floor = n) |> Seq.map fst |> Seq.toList

let stateIsValid (state:State) = [1..4] |> Seq.forall (fun n -> isValid (itemsOnFloor n state)) 

let getNextStates st = seq {
    let onThisFloor = itemsOnFloor st.floor st
    let newFloors = [st.floor+1;st.floor-1]
    //printfn "Onfloor %d, going to %A" st.floor newFloors
    for newFloor in newFloors do
        if newFloor >= 1 && newFloor <=4 then 
            for [a;b] in comb 2 onThisFloor do
                let mutable yieldedPair = false // failed attempt at optimization, has no effect if declared inside loop
            
                let canGoInElevator,isPair = match a,b with
                                                | Gen g, Chip m -> g = m, true
                                                | Chip m, Gen g -> m = g,true
                                                | _ -> true,false // 2 microchips or 2 generators
                let newState ={ floor = newFloor; items = st.items.Add(a,newFloor).Add(b,newFloor)  }
                if canGoInElevator && stateIsValid newState && (not (isPair && yieldedPair)) then
                    //printfn "taking %A and %A to floor %d" a b newFloor
                    yieldedPair <- true
                    yield newState    
            for item in onThisFloor do
                let newState = { floor = newFloor; items = st.items.Add (item,newFloor) }
                if stateIsValid newState then
                    //printfn "taking %A to floor %d" item newFloor
                    yield newState 
}

let getUnseenNextStates (seen:HashSet<State>) n st =
    getNextStates st
    |> Seq.filter (fun s -> seen.Contains(s) |> not)
    |> Seq.map (fun s -> seen.Add(s) |> ignore
                         s,(n+1))

let solve startState =
    let seen = new HashSet<State>()
    let statesQueue = new Queue<State*int>()
    let rec solveBreadthFirst() =
        if statesQueue.Count = 0 then
            failwith "no solution"
        else
            let (head,n) = statesQueue.Dequeue()
            if isSolution head then n
            else 
                for c in getUnseenNextStates seen n head do
                    statesQueue.Enqueue c
                solveBreadthFirst()
    let sw = System.Diagnostics.Stopwatch.StartNew()
    statesQueue.Enqueue (startState,0)
    let sol = solveBreadthFirst() 
    printfn "%dms" sw.ElapsedMilliseconds
    sol                        

let testState = { floor = 1; items = [ Chip "h", 1; Chip "l", 1; Gen "h", 2; Gen "l", 3 ] |> Map.ofList } 
let startState = { floor = 1; items = [
                                        Gen "promethium", 1
                                        Chip "promethium", 1
                                        Gen "cobalt", 2
                                        Gen "curium", 2
                                        Gen "ruthenium", 2
                                        Gen "plutonium", 2
                                        Chip "cobalt", 3
                                        Chip "curium", 3
                                        Chip "ruthenium", 3
                                        Chip "plutonium", 3
                                    ] |> Map.ofList}


solve testState |> printfn "Test: %d" // 11
solve startState |> printfn "Part a: %d" // 33   
solve  {startState with items= startState.items
                                                        .Add(Gen "elerium", 1)
                                                        .Add(Chip "elerium", 1)
                                                        .Add(Gen "dilithium", 1)
                                                        .Add(Chip "dilithium", 1)
                                                        }  |> printfn "Part b: %d"    // 57
