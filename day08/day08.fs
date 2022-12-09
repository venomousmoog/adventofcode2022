
let lines = System.IO.File.ReadLines "data.txt"
//let lines = System.IO.File.ReadLines "test.txt"

// part 1
let trees = array2D lines

let surroundings (map:'T[,]) (i,j) =
    let ra = Array.rev map[i, ..j-1] 
    let rb = map[i, j+1..]
    let ca = Array.rev map[..i-1, j] 
    let cb = map[i+1.., j]
    [ra; rb; ca; cb] 
    
let visible (map:'T[,]) (i,j) = 
    let h = map[i,j]
    surroundings map (i,j)
    |> Seq.exists (Seq.forall (fun t -> (t < h)))

(seq{0..trees.GetLength(0)-1}, seq{0..trees.GetLength(1)-1})
    ||> Seq.allPairs
    |> Seq.filter (visible trees)
    |> Seq.length
    |> printfn "%A"

// part 2
let scenic (map:'T[,]) (i,j) = 
    let h = map[i,j]
    let folder s' e =
        match s' with
        | (true, s) when e < h -> (true, s + 1) 
        | (true, s) -> (false, s + 1)
        | (false, s) -> (false, s)

    let score r = snd (r |> Seq.fold folder (true, 0))
    surroundings map (i,j)
    |> Seq.map score

(seq{0..trees.GetLength(0)-1}, seq{0..trees.GetLength(1)-1})
    ||> Seq.allPairs
    |> Seq.map (scenic trees)
    |> Seq.map (Seq.fold (fun s e -> s * e) 1)
    |> Seq.max
    |> printfn "%A"

