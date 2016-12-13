// breadth first solver, optimized enough to get part b solved in <20 mins. Lots more scope for speedup!
open System.Collections.Generic;
type Item = Gen of string | Chip of string
type State = { floor: int; items: Map<Item, int>  }

let isValid items =
    let hasGenerator t = Seq.contains (Gen t) items
    let noGenerators = Seq.forall (function | Chip _ -> true | _ -> false)
    let allMicrochipsMatch = Seq.forall (function | Chip m -> hasGenerator m | _ -> true) 
    noGenerators items || allMicrochipsMatch items

let isSolution state = state.items |> Map.forall (fun _ floor -> floor = 4)

let itemsOnFloor n state = state.items |> Map.toSeq |> Seq.filter (fun (item,floor) -> floor = n) |> Seq.map fst |> Seq.toArray

let stateIsValid (state:State) = Seq.forall (fun n -> isValid (itemsOnFloor n state)) 

let getNextStates st = seq {
    let onThisFloor = itemsOnFloor st.floor st
    let canGoInElevator = function | Gen g, Chip m -> g = m | Chip m, Gen g -> m = g | _ -> true // 2 microchips or 2 generators

    let pairs = [for i in 0..onThisFloor.Length-1 do 
                 for j in i+1..onThisFloor.Length-1 do
                 let a,b =onThisFloor.[i],onThisFloor.[j]
                 if canGoInElevator (a,b) then yield [a;b]
                 ]
    let getStates newFloor moves = seq {
        for move in moves do 
            let newItems = move |> List.fold (fun (items:Map<Item,int>) move -> items.Add(move,newFloor)) st.items
            let newState = {  floor = newFloor; items = newItems  }
            if stateIsValid newState [st.floor;newFloor] then
                yield newState
    }

    // todo: restore can go in elevator? maybe irrelevant
    if st.floor < 4 then
        yield! getStates (st.floor+1) pairs
        yield! getStates (st.floor+1) (onThisFloor |> Seq.map (fun a -> [a]))
    if st.floor > 1 then
        yield! getStates (st.floor-1) (onThisFloor |> Seq.map (fun a -> [a]))
        yield! getStates (st.floor-1) pairs
}

let getUnseenNextStates (seen:HashSet<State>) st =
    getNextStates st
    |> Seq.filter (fun s -> seen.Add(s))

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
                for c in getUnseenNextStates seen head do
                    statesQueue.Enqueue (c, n+1)
                solveBreadthFirst()
    let sw = System.Diagnostics.Stopwatch.StartNew()
    statesQueue.Enqueue (startState,0)
    printfn "%A" System.DateTime.Now
    let sol = solveBreadthFirst() 
    printfn "%A" sw.Elapsed
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
