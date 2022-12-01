open System.IO;

//let lines = File.ReadLines("mini.txt")
let lines = File.ReadLines("data.txt")
//lines |> Seq.iter(fun x -> printfn  "%s" x)

let split lst =
  let rec f lst acc last =
    match lst with
    | [] -> List.rev (last::acc)
    | ""::lst' -> f lst' (last::acc) []  
    | x::lst' -> f lst' acc (x::last)
  f lst [] []

split (List.ofSeq lines) 
    |> Seq.map(fun elf -> Seq.sumBy int elf)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum
    |> printfn "%A"
