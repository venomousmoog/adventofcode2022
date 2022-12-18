let lines = System.IO.File.ReadLines "data.txt"
//let lines = System.IO.File.ReadLines "test.txt"

let segments = 
    lines
    |> Seq.toList
    |> List.map (fun s -> s.Split([|' '; '-'; '>'|], System.StringSplitOptions.RemoveEmptyEntries))
    |> List.map (Seq.map (fun s -> let pair = s.Split(',') in (int (pair[0]), int (pair[1]))))
    |> List.map (Seq.fold (fun (a, p) e -> 
                            match p with
                            | None -> ([], Some e)
                            | Some x -> (a@[(x,e)], Some e)) ([], None))
    |> List.map (fun f -> Seq.toList (fst f))
    |> List.fold List.append []

let bounds = 
    let ext = segments |> Seq.append [((500,0),(500,0))]
    let xmin = ext |> Seq.map (fun ((xa,_), (xb,_)) -> min xa xb) |> Seq.min 
    let ymin = ext |> Seq.map (fun ((_,ya), (_,yb)) -> min ya yb) |> Seq.min 
    let xmax = ext |> Seq.map (fun ((xa,_), (xb,_)) -> max xa xb) |> Seq.max 
    let ymax = ext |> Seq.map (fun ((_,ya), (_,yb)) -> max ya yb) |> Seq.max
    ((xmin-1, ymin-1), (xmax+1, ymax+1))

// build our array map
let map = 
    segments
    |> Seq.map (fun ((xa, ya), (xb, yb)) -> 
        match ((xa, ya), (xb, yb)) with
        | ((xa, ya), (xb, yb)) when xa = xb -> seq { (min ya yb)..(max ya yb) } |> Seq.map (fun y -> (xa, y))
        | ((xa, ya), (xb, yb)) when ya = yb -> seq { (min xa xb)..(max xa xb) } |> Seq.map (fun x -> (x, ya))
        | _ -> invalidOp "woopsy")
    |> Seq.map (Seq.fold (fun s e -> Set.add e s) Set.empty)
    |> Set.unionMany

let display map =
    let xs = map |> Seq.map fst |> Seq.append [500]
    let ys = map |> Seq.map snd |> Seq.append [0]
    let xmax = Seq.max xs
    let xmin = Seq.min xs
    let ymax = Seq.max ys
    let ymin = Seq.min ys

    seq {ymin..ymax} 
    |> Seq.iter (fun y -> 
        seq {xmin..xmax}
        |> Seq.map (fun x -> 
                    match Set.contains (x,y) map with
                    | true -> '#'
                    | false -> '.')
        |> System.String.Concat
        |> printfn "%d: %s" y)

let ((_, _), (_, floor)) = bounds

// write something to place a new point of sand in the map:
let move map (x, y) floor =
    seq { (x,y+1); (x-1,y+1); (x+1,y+1) }
    |> Seq.tryFind (fun (x',y') -> not (Set.contains (x',y') map) && y' <= floor)

let rec place map (x,y) floor=
    match move map (x,y) floor with
    | None -> (x, y)
    | Some((x',y')) -> place map (x', y') floor

let rec search map floor count =
    match place map (500,-1) floor with
    | (500, -1) -> count
    | (_, y) when y = floor -> count
    | v -> search (Set.add v map) floor count+1

search map floor 0 |> printfn "%A"

// part 2
