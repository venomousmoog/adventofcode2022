
let lines = System.IO.File.ReadLines "data.txt"
//let lines = System.IO.File.ReadLines "test.txt"

// part1 
let packets n (s, a) e =
    if (Seq.length a) >= n then
        let lst = (List.skip 1 a)@[e]
        ((s+1, lst), (s+1, lst))
    else
        ((s, a@[e]), (s, a@[e]))

(Seq.head lines)
    |> Seq.mapFold (packets 4) (0, [])
    |> fst
    |> Seq.find (fun p -> (Seq.length (Set (snd p))) = 4)
    |> (fun a -> (fst a) + 4)
    |> printfn "start of packet: %A"

// part4
(Seq.head lines)
    |> Seq.mapFold (packets 14) (0, [])
    |> fst
    |> Seq.find (fun p -> (Seq.length (Set (snd p))) = 14)
    |> (fun a -> (fst a) + 14)
    |> printfn "start of packet: %A"