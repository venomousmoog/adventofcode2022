
let lines = System.IO.File.ReadLines "data.txt"
//let lines = System.IO.File.ReadLines "test.txt"

// part 1
let stackStrings = 
    lines
    |> Seq.takeWhile (fun line -> line <> "")
    |> Seq.rev

let stackCount = (Seq.head stackStrings).Split(" ", System.StringSplitOptions.RemoveEmptyEntries).Length

let cols = seq { for i in 0..stackCount-1 -> 1 + 4*i}

let extractStack stacks col =
    stacks
    |> Seq.skip 1
    |> Seq.filter (fun (s:string) -> col < s.Length && s[col] <> ' ')
    |> Seq.map (fun s -> s[col])
    |> Seq.toList

let stacks =
    Seq.toArray cols
    |> Array.map (extractStack stackStrings)

let pop n stack =
    let remaining = (List.length stack) - n
    (List.take remaining stack, List.rev (List.skip remaining stack))

let push sq stack =
    List.append stack sq

let moves = 
    Seq.skip ((Seq.length stackStrings) + 1) lines

let applyMove popper (stacks:array<list<char>>) (move:string) =
    let arr = move.Split()
    let count = int arr[1]
    let source = (int arr[3])-1
    let target = (int arr[5])-1
    let (updatedSrc, removed) = popper count (stacks[source])
    let updateStack (i:int) s =  
        match i with
        | _ when i = source -> updatedSrc
        | _ when i = target -> push removed s
        | _ -> s
    stacks 
        |> Array.mapi updateStack

moves
    |> Seq.fold (applyMove pop) stacks
    |> Seq.map Seq.last
    |> System.String.Concat
    |> printfn "%A"

// part 2
let pop2 n stack =
    let remaining = (List.length stack) - n
    (List.take remaining stack, List.skip remaining stack)

moves
    |> Seq.fold (applyMove pop2) stacks
    |> Seq.map Seq.last
    |> System.String.Concat
    |> printfn "%A"
