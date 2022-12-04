let lines = System.IO.File.ReadLines "data.txt"
//let lines = System.IO.File.ReadLines "test.txt"

type Range<'a> = {l:'a; u:'a}

// part1 
let pairs = 
    lines
    |> Seq.map (fun l -> l.Split(",") 
                         |> Array.map (fun r -> 
                                let s = r.Split("-") in 
                                {l = int s[0]; u = int s[1]}))

let fullyOverlap a b = 
    if (a.l <= b.l && a.u >= b.u) || (b.l <= a.l && b.u >= a.u) then true else false 

pairs
    |> Seq.filter (fun f -> fullyOverlap f[0] f[1])
    |> Seq.length
    |> printfn "fullyOverlap: %A"

// part2
let partialOverlap a b = 
    if ((a.l >= b.l && a.l <= b.u) 
        || (a.u >= b.l && a.u <= b.u)
        || (b.l >= a.l && b.l <= a.u)
        || (b.u >= a.l && b.u <= a.u)) then true else false 

pairs
    |> Seq.filter (fun f -> partialOverlap f[0] f[1])
    |> Seq.length
    |> printfn "partialOverlap: %A"

