let testInput = [| 
                "eedadn"
                "drvtee"
                "eandsr"
                "raavrd"
                "atevrs"
                "tsrnev"
                "sdttsa"
                "rasrtv"
                "nssdts"
                "ntnada"
                "svetve"
                "tesnvt"
                "vntsnd"
                "vrdear"
                "dvrsen"
                "enarar"
                |]

let mostFrequentAtPos n =
    testInput |> Seq.map (fun msg -> msg.[n]) |> Seq.countBy id |> Seq.sortByDescending (fun (a,b) -> (b,a))

mostFrequentAtPos 0