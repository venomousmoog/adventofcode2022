// Load the test (or real) data
let plays = 
//    (System.IO.File.ReadLines "test.txt")
    (System.IO.File.ReadLines "data.txt")
    |> Seq.map(fun line -> let split = (line.Split " ") in (split[0], split[1]))

// Part 1
let scores = Map [ ("X", 1); ("Y", 2); ("Z", 3) ]
let matches = Map [("A", "X"); ("B", "Y"); ("C", "Z")]
let wins = Map [("A", "Y"); ("B", "Z"); ("C", "X")]
let loses = Map [("A", "Z"); ("B", "X"); ("C", "Y")]

let score play = 
    match play with
        | (opponent, you) when wins[opponent] = you -> 6 + (int scores[you])    // win
        | (opponent, you) when matches[opponent] = you -> 3 + (int scores[you]) // draw
        | (_, you) -> 0 + (int scores[you])                                     // loss

plays
    |> Seq.map(score)
    |> Seq.sum
    |> printfn "%A"

// Part 2:
let strategy play = 
    match play with
        | (opponent, "X") -> (opponent, loses[opponent])   // lose
        | (opponent, "Y") -> (opponent, matches[opponent]) // draw
        | (opponent, "Z") -> (opponent, wins[opponent])    // win
        | _ -> invalidOp "unexpected input"

plays
    |> Seq.map(strategy)
    |> Seq.map(score)
    |> Seq.sum
    |> printfn "%A"
