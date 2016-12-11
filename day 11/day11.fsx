type Item = Generator of string | Microchip of string

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
    let hasGenerator t = List.contains (Generator t) items
    let noGenerators = items |> List.forall (function | Microchip _ -> true | _ -> false)
    let allMicrochipsMatch = items |> List.forall (function | Microchip m -> hasGenerator m | _ -> true) 
    noGenerators || allMicrochipsMatch

let isSolution state = state.items |> Map.toSeq |> Seq.forall (fun (_,floor) -> floor = 4)

let itemsOnFloor n state = state.items |> Map.toSeq |> Seq.filter (fun (item,floor) -> floor = n) |> Seq.map fst |> Seq.toList

let stateIsValid (state:State) = [1..4] |> Seq.forall (fun n -> isValid (itemsOnFloor n state)) 

let getNextStates st = seq {
    let onThisFloor = itemsOnFloor st.floor st
    let newFloors = [st.floor+1;st.floor-1]
    //printfn "Onfloor %d, going to %A" st.floor newFloors
    for newFloor in newFloors do
        if newFloor >= 1 && newFloor <=4 then 
            for [a;b] in comb 2 onThisFloor do
                let canGoInElevator = match a,b with
                                        | Generator g, Microchip m -> g = m
                                        | Microchip m, Generator g -> m = g
                                        | _ -> true // 2 microchips or 2 generators
                let newState ={ floor = newFloor; items = st.items.Add(a,newFloor).Add(b,newFloor)  }
                if canGoInElevator && stateIsValid newState then
                    //printfn "taking %A and %A to floor %d" a b newFloor
                    yield newState    
            for item in onThisFloor do
                let newState = { floor = newFloor; items = st.items.Add (item,newFloor) }
                if stateIsValid newState then
                    //printfn "taking %A to floor %d" item newFloor
                    yield newState 
}

let mutable best = 100 // System.Int32.MaxValue
let rec solve (currentState:State) (pastStates:State list) = seq {
    let stepsSoFar =List.length pastStates
    if stepsSoFar < best then
        //printfn "STEP %d on floor %d" stepsSoFar currentState.floor
        for state in getNextStates currentState do
            if isSolution state then
                best <- stepsSoFar + 1
                printfn "solved %d" best
                yield (state::pastStates)
            else if List.contains state pastStates then
                ()
            else
                yield! solve state (state::pastStates)
}

let testState = { floor = 1; items = [ 
                                Microchip "hydrogen", 1
                                Microchip "lithium", 1
                                Generator "hydrogen", 2
                                Generator "lithium", 3    
                            ] |> Map.ofList } 
let startState = { floor = 1; items = [
                                        Generator "promethium", 1
                                        Microchip "promethium", 1
                                        Generator "cobalt", 2
                                        Generator "curium", 2
                                        Generator "ruthenium", 2
                                        Generator "plutonium", 2
                                        Microchip "cobalt", 3
                                        Microchip "curium", 3
                                        Microchip "ruthenium", 3
                                        Microchip "plutonium", 3
                                    ] |> Map.ofList}


let solution = solve startState [] |> Seq.minBy List.length 
//solution |> Seq.find (fun _ -> true) |> (printfn "%A")
solution |> printfn "%A"  // List.length |> printfn "%d" 
solution |> List.length |> printfn "part a: %d"


//let nextState = getNextStates startState |> Seq.toArray |> printfn "%A"
