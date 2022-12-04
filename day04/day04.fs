let lines = System.IO.File.ReadLines "data.txt"
//let lines = System.IO.File.ReadLines "test.txt"

// part1 
type Range<'a> = {l:'a; u:'a}

let workerPairs (lines:seq<string>) = 
    let toRange (r:string) = let s = r.Split("-") in {l = int s[0]; u = int s[1]}
    let toRanges (l:string) = l.Split(",") |> Array.map toRange
    lines |> Seq.map toRanges

let fullyOverlap a b =
    let chk r1 r2 = (r1.l <= r2.l && r1.u >= r2.u) 
    chk a b || chk b a

lines 
    |> workerPairs
    |> Seq.filter (fun f -> fullyOverlap f[0] f[1])
    |> Seq.length
    |> printfn "fullyOverlap: %A"

// part2
let partialOverlap a b = 
    let chk i r = (i >= r.l && i <= r.u) 
    chk a.l b || chk a.u b || chk b.l a || chk b.u a

lines
    |> workerPairs
    |> Seq.filter (fun f -> partialOverlap f[0] f[1])
    |> Seq.length
    |> printfn "partialOverlap: %A"

