open System.IO;

//let lines = File.ReadLines("test.txt")
let lines = File.ReadLines("data.txt")

let split lst = 
    let folder (grouped, accum) element = 
        match element with
        | "" -> (grouped@[accum], [])
        | x -> (grouped, accum@[x])
    fst (Seq.fold folder ([], []) lst)

split lines 
    |> Seq.map (fun elf -> Seq.sumBy int elf)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum
    |> printfn "%A"
