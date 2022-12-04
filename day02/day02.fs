// Load the test (or real) data
//let lines = System.IO.File.ReadLines "test.txt"
let lines = System.IO.File.ReadLines "data.txt"

// part1
type Play = Rock | Paper | Scissors

let toPlay str = 
    match str with 
        | "A" | "X" -> Rock 
        | "B" | "Y" -> Paper
        | "C" | "Z" -> Scissors
        | _ -> invalidArg "str" "unexpected string"

let wins play = match play with | Rock -> Paper | Paper -> Scissors | Scissors -> Rock
let loses play = match play with Rock -> Scissors | Paper -> Rock | Scissors -> Paper
let playScore play = match play with Rock -> 1 | Paper -> 2 | Scissors -> 3

let score play = 
    match play with
        | (opponent, you) when (wins opponent) = you -> 6 + (playScore you) // win
        | (opponent, you) when opponent = you -> 3 + (playScore you)        // draw
        | (_, you) -> 0 + (playScore you)                                   // loss

lines
    |> Seq.map (fun s -> let e = s.Split() in (toPlay e[0], toPlay e[1]))
    |> Seq.map(score)
    |> Seq.sum
    |> printfn "%A"

//// Part 2:
let strategy play = 
    match play with
        | (opponent, "X") -> (opponent, loses opponent)  // lose
        | (opponent, "Y") -> (opponent, opponent)        // draw
        | (opponent, "Z") -> (opponent, wins opponent)   // win
        | _ -> invalidOp "unexpected input"

lines
    |> Seq.map (fun s -> let e = s.Split() in (toPlay e[0], e[1]))
    |> Seq.map(strategy)
    |> Seq.map(score)
    |> Seq.sum
    |> printfn "%A"

