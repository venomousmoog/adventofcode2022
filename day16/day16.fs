//let lines = System.IO.File.ReadLines "data.txt"
let lines = System.IO.File.ReadLines "test.txt"

let valve (s:string) =
    let [|before; after|] = s.Split(';', System.StringSplitOptions.TrimEntries)
    let splat = before.Split(' ', '=') in 
    (splat[1], (int splat[5], after.Split([|' '; ','|], System.StringSplitOptions.TrimEntries)[4..]))

let valves = 
    lines 
    |> Seq.map valve
    |> Map.ofSeq

valves
|> Map.iter (fun k (p, next) -> let sep = " " in printfn $"{k}: {p}, {String.concat sep next}")

let validMoves current visited = 
    snd valves[current] |> Array.filter (fun f -> not (Set.contains f visited)) |> Array.toList

let rec search current visited deadline =
    let searchRest d' = (validMoves current visited |> List.map (fun c -> search c (Set.add c visited) d'-1))
    match deadline with
    | 0 -> 0
    | 1 -> List.max ((fst valves[current])::searchRest deadline)
    | _ -> 
        let curp = (fst valves[current])
        List.max (List.append (searchRest deadline) ((searchRest (deadline-1)) |> List.map (fun f -> f + curp)))

search "AA" (Set.empty) 30
|> printfn "%A"