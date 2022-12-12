let lines = System.IO.File.ReadLines "data.txt"
//let lines = System.IO.File.ReadLines "test.txt"

// part 1
type Operator = Add | Multiply
type Operand = Old | Value of int

type Monkey = {
    operator: Operator
    operand: Operand
    test: int
    trueMonkey: int
    falseMonkey: int
    // ick
    mutable inspections: int
}

// let's load some monkeys
let loadMonkey (lines:string list) =
    let items = lines[1].Substring("  Starting items: ".Length).Split(", ") |> Array.map (fun is -> int is) |> Seq.toList
    let monkey = {
        operator = match lines[2]["  Operation: new = old ".Length] with 
                    | '+' -> Operator.Add 
                    | '*' -> Operator.Multiply
                    | c -> invalidOp $"unexpected {c}"
        operand = match lines[2].Substring("  Operation: new = old + ".Length) with 
                  | "old" -> Operand.Old
                  | i -> Operand.Value (int i)
        test = int (lines[3].Substring("  Test: divisible by ".Length))
        trueMonkey = int (lines[4].Substring("    If true: throw to monkey ".Length))
        falseMonkey = int (lines[5].Substring("    If false: throw to monkey ".Length))
        inspections = 0
    }
    (items, monkey)

let rec loadMonkeys lines =
    match lines with
    | [] -> []
    | l -> l 
           |> List.splitAt (min l.Length 7)
           |> (fun (monkey, rest) -> (loadMonkey monkey)::(loadMonkeys rest))

// and cache our global state
let (worryState, monkeys) = 
    let m = loadMonkeys (Seq.toList lines)
    ((List.map fst m), (List.map snd m))

let inspect monkey worry =
    monkey.inspections <- monkey.inspections + 1
    let operand = match monkey.operand with 
                  | Old -> worry 
                  | Value i -> i
    let worry' = (match monkey.operator with
                  | Add -> worry + operand
                  | Multiply -> worry * operand) / 3
    match worry' % monkey.test with
    | 0 -> (monkey.trueMonkey, worry')
    | _ -> (monkey.falseMonkey, worry')

let rebuild state' index i e = 
    match i with
    | _ when i = index -> []
    | s -> List.append e (match (Map.tryFind s state') with 
                          | Some a -> a
                          | None -> [])

let updateMonkey inspect (state:'a list list) index =
    let state' = state[index] 
                 |> List.map (inspect monkeys[index]) 
                 |> List.groupBy fst
                 |> List.map (fun (target, items) -> (target, items |> List.map snd))
                 |> Map
    state |> List.mapi (rebuild state' index)

let round inspect (state:'a list list) =
    seq {0..state.Length-1}
    |> Seq.fold (updateMonkey inspect) state

seq {0..19}
|> Seq.fold (fun s i -> round inspect s) worryState
|> Seq.iteri (fun i e -> printfn "monkey %A: %A" i e)

monkeys 
|> List.map (fun m -> m.inspections)
|> List.sortDescending
|> List.take 2
|> fun [a; b] -> printfn $"{a} * {b} = {(bigint a) * (bigint b)}" 

// part 2
monkeys |> Seq.iter (fun m -> (m.inspections <- 0))

let inspectMod modulus monkey worry =
    monkey.inspections <- monkey.inspections + 1
    let operand = match monkey.operand with 
                  | Old -> worry 
                  | Value i -> i
    let worry' = (match monkey.operator with
                  | Add -> (worry + operand) % modulus
                  | Multiply -> int ((bigint worry * bigint operand) % (bigint modulus)))
    match worry' % monkey.test with
    | 0 -> (monkey.trueMonkey, worry')
    | _ -> (monkey.falseMonkey, worry')

let modulus = monkeys |> Seq.fold (fun s m -> s * m.test) 1

seq {0..10_000-1}
|> Seq.fold (fun s i -> round (inspectMod modulus) s) worryState
|> Seq.iteri (fun i e -> printfn "monkey %A: %A" i e)

monkeys 
|> List.map (fun m -> m.inspections)
|> List.sortDescending
|> List.take 2
|> fun [a; b] -> printfn $"{a} * {b} = {(bigint a) * (bigint b)}" 
