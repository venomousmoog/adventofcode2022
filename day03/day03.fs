
let lines = System.IO.File.ReadLines "data.txt"
//let lines = System.IO.File.ReadLines "test.txt"

// part1
let priority c =
    match c with 
        | t when t >= 'a' && t <= 'z' -> int (t - 'a') + 1
        | t when t >= 'A' && t <= 'Z' -> int (t - 'A') + 27
        | _ -> invalidOp "unexpected input"

let split (s:string) =
    let left = s.[0..(s.Length/2)-1]
    let right = s.[s.Length/2..]
    (Set left, Set right)

lines 
    |> Seq.map split
    |> Seq.map(fun (a,b) -> Set.intersect a b)
    |> Seq.map Seq.head
    |> Seq.map priority
    |> Seq.sum
    |> printfn "%A"  

// part 2
let rec groupByCount n s = 
    seq {
        if not (Seq.isEmpty s) then
            yield Seq.take n s 
            yield! groupByCount n (Seq.skip n s)
    }

lines 
    |> Seq.map Set
    |> groupByCount 3
    |> Seq.map (Seq.reduce Set.intersect)
    |> Seq.map Seq.head
    |> Seq.map priority
    |> Seq.sum
    |> printfn "%A"
