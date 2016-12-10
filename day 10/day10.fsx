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

let instructions = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt") |> Array.map parseInstruction

//let state = instructions |> Seq.choose (function | StartsWith bot,chip -> Some (Bot int) | _ -> None)      

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
            printfn "give from bot %d" bot 
            let st2 = updateState st low lowChip
            updateState st2 high highChip            
        | _ ->
            //printfn "bot %d has only one chip" bot 
            st
    else
        //printfn "bot %d has nothing" bot 
        st 

let applyInstruction state instruction = 
    match instruction with
    | StartsWith (bot,chip) -> updateState state (Bot bot) chip 
    | GivesTo (bot,low,high) -> handleGive state bot low high

let s1 = instructions |> Array.fold applyInstruction Map.empty
let s2 = instructions |> Array.fold applyInstruction s1

s2 |> printfn "%A"

(*let mutable state = Map.empty

for i in instructions do
    match i with
    | StartsWith (bot,chip) -> state <- updateState state (Bot bot) chip 
    | GivesTo (bot,low,high) -> state <- handleGive state bot low high

printfn "%A" (state |> Map.filter (fun c -> *)