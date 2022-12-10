let lines = System.IO.File.ReadLines "data.txt"
//let lines = System.IO.File.ReadLines "test.txt"

// part 1
type Reg = { X: int; Clock: int }

let execute reg inst = 
    match inst with
    | [|"noop"|] -> { X = reg.X; Clock = reg.Clock + 1}
    | [|"addx"; i|] -> { X = reg.X + (int i); Clock = reg.Clock + 2}
    | _ -> invalidOp $"invalidOp {inst}"

let record reg inst = (reg, execute reg inst)

let trace = lines
            |> Seq.map (fun l -> l.Split())
            |> Seq.mapFold record {X = 1; Clock = 0}
            |> fst

let sample trace clock = 
    (Seq.findBack (fun {X = _; Clock = c } -> c <= (clock-1)) trace).X

let signal trace clock = 
    (sample trace clock) * clock

[20; 60; 100; 140; 180; 220]
    |> Seq.map (sample trace)
    |> Seq.sum
    |> (printfn "%A")

// part 2
let rec display s = 
    match s with
    | "" -> ()
    | s -> 
        printfn "%A" s[..39]
        display s[40..]

seq {1..240}
    |> Seq.map (fun c -> (c, (sample trace c)))
    |> Seq.map (fun (c, x) -> if (abs(((c-1)%40)-x) <= 1) then "#" else ".")
    |> String.concat ""
    |> display
