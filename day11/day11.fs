//let lines = System.IO.File.ReadLines "data.txt"
let lines = System.IO.File.ReadLines "test.txt"

// part 1

type Operator = Add | Multiply
type Operand = Old | Value of int

type Monkey = {
    operator: Operator
    operand: Operand
    test: bigint
    trueMonkey: int
    falseMonkey: int
    // ick
    mutable inspections: int
    mutable worryFactor: bigint
}

let loadMonkey (lines:string list) =
    let items = lines[1].Substring("  Starting items: ".Length).Split(", ") |> Array.map (fun is -> bigint (int is)) |> Seq.toList
    let monkey = {
        operator = match lines[2]["  Operation: new = old ".Length] with 
                    | '+' -> Operator.Add 
                    | '*' -> Operator.Multiply
                    | c -> invalidOp $"unexpected {c}"
        operand = match lines[2].Substring("  Operation: new = old + ".Length) with 
                  | "old" -> Operand.Old
                  | i -> Operand.Value (int i)
        test = bigint (int (lines[3].Substring("  Test: divisible by ".Length)))
        trueMonkey = int (lines[4].Substring("    If true: throw to monkey ".Length))
        falseMonkey = int (lines[5].Substring("    If false: throw to monkey ".Length))
        inspections = 0
        worryFactor = bigint 3
    }
    (items, monkey)

// let's load some monkeys
let rec loadMonkeys lines =
    match lines with
    | [] -> []
    | l -> l 
           |> List.splitAt (min l.Length 7)
           |> (fun (monkey, rest) -> (loadMonkey monkey)::(loadMonkeys rest))

let (worryState, monkeys) = 
    let m = loadMonkeys (Seq.toList lines)
    ((List.map fst m), (List.map snd m))

let inspect monkey (worry:bigint) =
    monkey.inspections <- monkey.inspections + 1
    let operand = match monkey.operand with 
                  | Old -> worry 
                  | Value i -> i
    let worry' = 
        (match monkey.operator with
         | Add -> worry + operand
         | Multiply -> worry * operand) / monkey.worryFactor
    if worry' % monkey.test = 0 then
        (monkey.trueMonkey, worry')
    else
        (monkey.falseMonkey, worry')

// each round computes all monkeys, but 
// items moved within the round (say from monkey 0
// to monkey 1) will be computed at the receiving
// monkey - so each monkey processed affects the state
// of monkeys for the next:
let rebuild state' index i e = 
    match i with
    | _ when i = index -> []
    | s -> List.append e (match (Map.tryFind s state') with 
                            | Some a -> a
                            | None -> [])

let updateMonkey (state: bigint list list) index =
    let state' = state[index] 
                 |> List.map (inspect monkeys[index]) 
                 |> List.groupBy fst
                 |> List.map (fun (target, items) -> (target, items |> List.map snd))
                 |> Map
    state |> List.mapi (rebuild state' index)

let round (state:bigint list list) =
    seq {0..state.Length-1}
    |> Seq.fold updateMonkey state

seq {0..19}
|> Seq.fold (fun s i -> round s) worryState
|> printfn "%A"

monkeys 
|> List.map (fun m -> m.inspections)
|> List.sortDescending
|> List.take 2
|> fun [a; b] -> a*b
|> printfn "%A"

// part 2
monkeys
|> Seq.iter (fun m -> (m.inspections <- 0; m.worryFactor <- 1))

seq {0..10000}
|> Seq.fold (fun s i -> ((if i%100 = 0 then printfn $"round {i}"); round s)) worryState
|> printfn "%A"

monkeys 
|> List.map (fun m -> m.inspections)
|> List.sortDescending
|> List.take 2
|> fun [a; b] -> a*b
|> printfn "%A"
