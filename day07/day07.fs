
let lines = System.IO.File.ReadLines "data.txt"
//let lines = System.IO.File.ReadLines "test.txt"

// part 1
type Entry =
    | File of name:string * size:int
    | Directory of name: string * children: Map<string, Entry>

    member this.name =
        match this with
        | File (name, _) -> name
        | Directory (name, _) -> name

let rec insert tree (path:list<string>) (node:Entry) =
    match tree with
    | Directory (name, children) -> match path with
                                    | [] -> Directory (name, (Map.add node.name node children))
                                    | segment::rest -> Directory (name, (Map.add segment (insert children[segment] rest node) children))
    | File _ -> invalidOp "can't insert child under a file"

let cd (cwd:list<string>) arg = 
    match arg with
    | ".." -> cwd[0..((List.length cwd)-2)]
    | "/" -> []
    | p -> cwd@[p]

let handleMove (tree, cwd) (line:string) = 
    match (line.Split()) with
    // handle listing (no changes)
    | [|"$"; "ls"|] -> (tree, cwd) 
    // change directory
    | [|"$"; "cd"; arg|] -> (tree, (cd cwd arg)) 
    // listing found a directory
    | [|"dir"; dir|] -> ((insert tree cwd (Directory (dir, Map.empty))), cwd) 
    // listing found a file
    | [|size; name|] -> ((insert tree cwd (File (name, int size))), cwd)
    // or we're confused
    | _ -> invalidOp $"unknown line type {line}" 

let disk = lines |> Seq.fold handleMove (Directory ("", Map.empty), []) |> fst

let rec size tree = 
    match tree with
    | Directory (_, children) -> (Seq.map size children.Values) |> Seq.sum
    | File (_, size) -> size

let rec flatten cwd tree = 
    match tree with
    | Directory (name, children) ->
        let cwd' = cwd + "/" + name
        let sizes = (List.collect (flatten cwd') (Seq.toList children.Values))
        (cwd', false, List.sumBy (fun (_, leaf, size) -> if leaf then size else 0) sizes)::sizes
    | File (name, size) -> [(name, true, size)]

disk
    |> flatten ""
    |> Seq.filter (fun (_, leaf, _) -> not leaf)
    |> Seq.filter (fun (_, _, size) -> size <= 100000)
    |> Seq.sumBy (fun (_, _, size) -> size)
    |> printfn "%A"

// part 2
let need = 30000000 - (70000000 - (size disk))

disk
    |> flatten ""
    |> Seq.filter (fun (_, leaf, _) -> not leaf)
    |> Seq.filter (fun (_, _, size) -> size >= need)
    |> Seq.sortBy (fun (_, _, size) -> size)
    |> Seq.head
    |> printfn "%A"

