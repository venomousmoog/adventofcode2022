let lines = System.IO.File.ReadLines "data.txt"
//let lines = System.IO.File.ReadLines "test.txt"

// part 1
let elevation = array2D lines

// lame
let findItems needle (arr: 'a[,]) =
    Seq.allPairs (seq {0..(Array2D.length1 arr)-1}) (seq {0..(Array2D.length2 arr)-1})
    |> Seq.filter (fun (x, y) -> arr[x, y] = needle)
    |> List.ofSeq

let findItem needle (arr: 'a[,]) = 
    match (findItems needle elevation) with
    | a::[] -> a
    | [] -> invalidOp "not found"
    | _::_ -> invalidOp "many matches"

let (sx, sy) = findItem 'S' elevation
let (ex, ey) = findItem 'E' elevation

elevation[sx, sy] <- 'a'
elevation[ex, ey] <- 'z'

let findValidMoves (x, y) visited = 
    let current = elevation[x,y]
    [ (0, 1); (0, -1); (-1, 0); (1, 0)] 
    |> List.map (fun (dx, dy) -> (x+dx, y+dy))
    |> List.filter (fun (x', y') -> (x' >= 0) && (y' >= 0) && (x' < (Array2D.length1 elevation)) && (y' < (Array2D.length2 elevation)))
    |> List.filter (fun (x', y') -> elevation[x',y'] <= (current+(char 1)))
    |> List.filter (fun p' -> not (Map.containsKey p' visited))

let rec pathTo ee (visited:Map<int*int, int*int>) = 
    match visited[ee] with
    | (-1,-1) -> [ee]
    | ee' -> ee::(pathTo ee' visited)

let rec findShortestPath queue (visited:Map<int*int, int*int>) =
  match queue with
  | []                              -> None
  | (ee)::_  when ee = (ex, ey) -> Some(pathTo ee visited)
  | (ee)::tail -> 
        let moves = (findValidMoves ee visited)
        findShortestPath (List.append tail moves) (moves |> Seq.fold (fun m e -> Map.add e ee m) visited)

match findShortestPath [(sx,sy)] (Map.empty.Add((sx,sy), (-1,-1))) with
| None -> printfn "oops"
| Some(p) -> printfn "%A: %A" (Seq.length p) p

// part 2
findItems 'a' elevation
|> Seq.map (fun (sx, sy) -> findShortestPath [(sx,sy)] (Map.empty.Add((sx,sy), (-1,-1))))
|> Seq.filter (fun a -> match a with None -> false | Some(_) -> true)
|> Seq.map (fun (Some a) -> Seq.length a)
|> Seq.sort
|> Seq.head
|> printfn "%A"