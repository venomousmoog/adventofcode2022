let lines = System.IO.File.ReadLines "data.txt"
//let lines = System.IO.File.ReadLines "test.txt"

type Token = StartList| EndList | Number of int

let (|StartsWith|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let rec tokens (s:string) = 
    Seq.toList (seq {
        match s with
        | "" -> ()
        | StartsWith "[" rest -> 
            yield StartList
            yield! (tokens rest)
        | StartsWith "]" rest -> 
            yield EndList
            yield! (tokens rest)
        | StartsWith "," rest | StartsWith " " rest-> 
            yield! (tokens rest)
        | s when System.Char.IsDigit s[0] -> 
            let str = System.String.Concat (Seq.takeWhile System.Char.IsDigit s)
            let value = int str
            yield Number(value)
            yield! (tokens (System.String.Concat (Seq.skipWhile System.Char.IsDigit s)))
        | _ -> invalidOp $"woopsy {s}"
    })

type Packet = PV of int | PL of list<Packet>

// this is ugly and smart people could probably do it better
let rec parseList (s:list<Token>) = 
    match s with
    | [] -> ([], PL([]))
    | EndList::rest -> (rest, PL([]))
    | StartList::rest -> 
        // parse child level
        let (afterChild, PL(child)) = parseList rest
        // parse the rest on this level
        let (r', PL(next)) = parseList afterChild
        // hand any remainder up
        (r', PL(PL(child)::next))
    | Number(x)::rest -> 
        // parse the rest on this level
        let (r', PL(next)) = parseList rest
        // hand any remainder up
        (r', PL(PV(x)::next))

let parse (s:list<Token>) = 
    let (remaining, parsed) = parseList s
    match parsed with
    | PL([x]) -> x
    | _ -> invalidOp $"unexpected result {parsed}"

let rec packetPairs (lines:list<string>) =
    match lines with
    | [] -> []
    | _ -> (parse(tokens lines[0]), parse(tokens lines[1]))::(packetPairs (List.skip (min 3 (List.length lines)) lines))

let pairs = packetPairs (Seq.toList lines)

type Check = Good | Bad | Continue

let rec check left right =
    match (left, right) with
    | (PV(l), PV(r)) when l < r -> Good
    | (PV(l), PV(r)) when l = r -> Continue
    | (PV(_), PV(_)) -> Bad
    | (PL(l), PL(r)) -> 
        let folder (score, l) r =
            match score with
            | Bad -> (Bad, [])
            | Good -> (Good, [])
            | Continue -> 
                match l with
                | [] -> (Good, [])
                | l'::lr -> (check l' r, lr)
        let (score, l') = Seq.fold folder (Continue, l) r
        match (score, l') with
        | (score, []) -> score
        | (Continue, _) -> Bad
        | _ -> score
    | (PL(l), PV(r)) -> (check (PL l) (PL [PV r]))
    | (PV(l), PL(r)) -> (check (PL [PV l]) (PL r))


// check each pair:
pairs
|> Seq.map (fun (l,r) -> check l r)
|> Seq.mapi (fun i e -> 
                printfn $"{i+1}: {e}"
                match e with
                | Good -> i+1
                | Bad -> 0
                | Continue -> invalidOp "woopsy")
|> Seq.sum
|> printfn "%A"

let compare l r =
    match check l r with
    | Good -> -1
    | Continue -> 0
    | Bad -> 1

pairs
|> List.map (fun (a,b) -> [a; b])
|> List.fold List.append []
|> List.append [(PL [PL [PV 2] ]); (PL [PL [PV 6] ])]
|> List.sortWith compare 
|> Seq.iteri (fun i e -> (printfn $"{i+1}: {e}"))
 