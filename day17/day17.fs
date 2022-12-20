
// split the list into a list of lists of strings, one list of strings
// for each of our elves
let split lst = 
    // folder takes an accumulation value (here, the resulting grouped lists and the
    // in progress list accum) and a new element, and returns a new accumulator
    let folder (grouped, accum) element = 
        match element with
        | "" -> (grouped @ [accum], [])
        | x -> (grouped, accum @ [x])
    // and run the accumulation function across the list and return the first element (grouped)
    fst (Seq.fold folder ([], []) lst)

// load some elfs
let elfs = split (System.IO.File.ReadLines "data.txt")

// part 1
elfs
    // total each elf
    |> Seq.map (Seq.sumBy int)
    // and find the max
    |> Seq.max
    |> printfn "%A"

// part 2
elfs
    // total each elf
    |> Seq.map (Seq.sumBy int)
    // sort the resulting list (largest to smallest)
    |> Seq.sortDescending
    // take the top 3 items
    |> Seq.take 3
    // and sum them up!
    |> Seq.sum
    |> printfn "%A"
