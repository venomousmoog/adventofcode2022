
let lines = System.IO.File.ReadLines "data.txt"
//let lines = System.IO.File.ReadLines "test2.txt"

// part 1

// apply a move to the head of the rope
let shift (hx, hy) move =
    match move with
    | "L" -> (hx - 1, hy)
    | "R" -> (hx + 1, hy)
    | "U" -> (hx, hy + 1)
    | "D" -> (hx, hy - 1)
    | _ -> invalidArg "move" $"unexpected move {move}"

// let the tail catch up:
let catch (hx:int, hy:int) (tx:int, ty:int) =
    let dx = hx - tx
    let dy = hy - ty

    let dir a = if a < 0 then -1 else 1

    match (dx, dy) with
    | (x, y) when abs(x) > 1 && y = 0 -> (tx + dir x, ty) // two away left/right
    | (x, y) when abs(y) > 1 && x = 0 -> (tx, ty + dir y) // two away up/down
    | (x, y) when (abs(x) > 1 && abs(y) >= 1) ||(abs(y) > 1 && abs(x) >= 1) -> (tx + dir x, ty + dir y) // diagonal
    | (x, y) when abs(x) > 1 && abs(y) > 1 -> invalidOp $"unexpected distance between {hx},{hy} and {tx},{ty}"
    | _ -> (tx, ty) // no move

let applyMove (head, tail) move =
    let h' = shift head move
    let t' = catch h' tail
    (t', (h', t'))

lines
    |> Seq.map (fun f -> f.Split())
    |> Seq.map (fun [|a;b|] -> (a, int b))
    |> Seq.collect (fun (a,b) -> seq {for _ in 1..b -> a})
    |> Seq.mapFold applyMove ((0,0), (0,0))
    |> fst
    |> Seq.distinct
    |> Seq.length
    |> (printfn "%A")

// part 2
let applyMove2 (head, rest) move =
    // apply the move to the head, and then catch up the rest of the knots
    let head' = shift head move
    let rest' = fst (rest |> Seq.mapFold (fun head' tail -> let tail' = catch head' tail in (tail', tail')) head')
    ((Seq.last rest'), (head', rest'))

lines
    |> Seq.map (fun f -> f.Split())
    |> Seq.map (fun [|a;b|] -> (a, int b))
    |> Seq.collect (fun (a,b) -> seq {for _ in 1..b -> a})
    |> Seq.mapFold applyMove2 ((0,0), seq{for _ in 1..9 -> (0,0)})
    |> fst
    |> Seq.distinct
    |> Seq.length
    |> (printfn "%A")
