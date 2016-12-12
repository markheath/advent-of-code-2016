// breadth first solver, can do part a, still needs optimizing for part b
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
                                        | Gen g, Chip m -> g = m
                                        | Chip m, Gen g -> m = g
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

let seen = new System.Collections.Generic.HashSet<State>()
let getUnseenNextStates n st =
    getNextStates st
    |> Seq.filter (fun s -> seen.Contains(s) |> not)
    |> Seq.map (fun s -> seen.Add(s) |> ignore
                         s,(n+1))
    |> Seq.toList


let rec solveBreadthFirst (statesQueue:(State*int) list) =
    match statesQueue with
    | [] -> failwith "no solution"
    | (head,n)::tail -> if isSolution head then n
                        else 
                            let children = getUnseenNextStates n head 
                            solveBreadthFirst (List.append tail children)


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


solveBreadthFirst [testState,0] |> printfn "Test: %d" // 11
//solveBreadthFirst [startState,0] |> printfn "Part a: %d" // 33   
solveBreadthFirst [ {startState with items= startState.items
                                                        .Add(Gen "elerium", 1)
                                                        .Add(Chip "elerium", 1)
                                                        .Add(Gen "dilithium", 1)
                                                        .Add(Chip "dilithium", 1)
                                                        } ,0] |> printfn "Part b: %d"    


