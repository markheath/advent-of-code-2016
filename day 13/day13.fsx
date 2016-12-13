let countBits number = 
    let rec countBits' count number =
        if number = 0 then count
        else
            let set = number &&& 0x1 = 1 
            countBits' (count + if set then 1 else 0)  (number >>> 1)
    countBits' 0 number

let isWall favourite (x,y) =
    if x < 0 || y < 0 then true
    else
        let n = x*x + 3*x + 2*x*y + y + y*y
        (countBits (n + favourite)) % 2 = 1


//countBits 5

//isWall 1350 400 400

open System.Collections.Generic
let search (start:int*int) isTarget (isWall:int*int->bool) =
    let points = new Queue<int*int>()
    let distances = new Dictionary<int*int,int>()
    let rec search'() =
        if points.Count = 0 then
            -1, distances
        else
            let (x,y) = points.Dequeue()
            let steps = distances.[(x,y)]
            if isTarget (x,y) steps then
                steps, distances
            else
                let newPoints = [(x+1,y);(x-1,y);(x,y+1);(x,y-1)]
                for newPoint in newPoints do
                    if not (distances.ContainsKey(newPoint)) then
                        if isWall newPoint then 
                            distances.[newPoint] <- System.Int32.MaxValue
                        else
                            distances.[newPoint] <- steps + 1
                            points.Enqueue(newPoint)
                search'()
    distances.[start] <- 0
    points.Enqueue(start)
    search'()



search (1,1) (fun a b -> a = (7,4)) (isWall 10) |> fst // 11
search (1,1) (fun a b -> a = (31,39)) (isWall 1350) |> fst // part a: 92

search (1,1) (fun a b -> b = 51) (isWall 1350) 
    |> snd 
    |> (fun a -> a.Values)
    |> Seq.filter (fun a -> a <= 50) 
    |> Seq.length
   // part b: 124

let input = 1350
let start = (1,1)
let target = (31,39)
