type Target = Bot of int | Output of int
type Instruction = StartsWith of int*int | GivesTo of int*Target*Target

let asTarget = function
    | [ "bot"; value ] -> Bot (int value)
    | [ "output"; value ] -> Output (int value)
    | _ -> failwith "invalid target"

let parseInstruction (inst:string) =
    let parts = inst.Split(' ')
    if parts.[0] = "value" then StartsWith(int parts.[1],int parts.[5])
    else GivesTo(int parts.[1], asTarget [parts.[5];parts.[6]], asTarget [parts.[10];parts.[11]])

let updateState (state:Map<Target,int list>) target chip =
    if Map.containsKey target state then
        let chips = state.[target]
        state.Add(target, chip::chips)
    else 
        state.Add(target, [chip])

let handleGive (st:Map<Target,int list>) bot low high =
    if Map.containsKey (Bot bot) st then
        match st.[Bot bot] with
        | [a;b] -> 
            let lowChip = min a b 
            let highChip = max a b
            if (lowChip = 17 && highChip = 61) then
                printfn "part a: %d" bot
            //printfn "give from bot %d" bot 
            let st2 = updateState st low lowChip
            Some (updateState st2 high highChip)            
        | _ ->
            //printfn "bot %d has only one chip %A" bot (st.[Bot bot]) 
            None
    else
        //printfn "bot %d has nothing" bot 
        None

let applyInstruction (state,leftovers) instruction = 
    let result = match instruction with
                    | StartsWith (chip, bot) -> Some (updateState state (Bot bot) chip) 
                    | GivesTo (bot,low,high) -> handleGive state bot low high
    match result with
        | Some st -> (st,leftovers)
        | None -> (state,instruction::leftovers)

let rec applyAll state instructions =
    let (newState,leftovers)  = instructions |> List.fold applyInstruction (state, [])
    match leftovers with 
    | [] -> newState 
    | _ ->
        if List.length leftovers = List.length instructions then
            printfn "Giving up"
            newState    
        else
            applyAll newState (leftovers |> List.rev)
      
let instructions = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt") |> Array.map parseInstruction
let finalState = applyAll Map.empty (instructions |> List.ofArray)
[0;1;2] |> List.collect (fun n -> finalState.[Output n]) |> List.reduce (*) |> printfn "part b: %d" // 12803


[StartsWith (5,2); 
 GivesTo (2, Bot 1, Bot 0); 
 StartsWith(3,1); 
 GivesTo (1, Output 0, Bot 0);
 GivesTo (2, Output 2, Output 0);
 StartsWith (2,2)] |> applyAll Map.empty |> printfn "TEST %A"