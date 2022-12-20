let lines = Seq.toList (System.IO.File.ReadLines "data.txt")
//let lines = Seq.toList (System.IO.File.ReadLines "test.txt")

let sb =
    let coord (s:string) =
        s.Split([|'x'; '='; ' '; 'y'; ','|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun se -> int se)
        |> Seq.toArray
        |> (fun [|a;b|] -> (a,b))
    lines
    |> List.map (fun s -> s.Split(':'))
    |> List.map (fun [|s;b|] -> (s.Substring("Sensor at x=".Length), b.Substring(" closest beacon is at ".Length)))
    |> List.map (fun (s, b) -> (coord s, coord b))

let sensors = Set (sb |> Seq.map fst)
let beacons = Set (sb |> Seq.map snd)

// part 1
let visible (sx, sy) dist =
    Set (seq {
        yield (sx, sy)
        for d in 1..dist do
            for x in 0..d do 
                yield (sx + x, sy + (d - x))
                yield (sx + x, sy - (d - x))
                yield (sx - x, sy + (d - x))
                yield (sx - x, sy - (d - x))
        })

let display map =
    let xs = map |> Seq.map fst
    let ys = map |> Seq.map snd
    let xmax = Seq.max xs
    let xmin = Seq.min xs
    let ymax = Seq.max ys
    let ymin = Seq.min ys
    printfn $"bounds = {xmin}, {ymin} -> {xmax}, {ymax}"

    seq {ymin..ymax} 
    |> Seq.iter (fun y -> 
        seq {xmin..xmax}
        |> Seq.map (fun x -> 
                    match Set.contains (x,y) sensors with
                    | true -> 'S' 
                    | false -> 
                        match Set.contains (x,y) beacons with
                        | true -> 'B' 
                        | false -> 
                            match Set.contains (x,y) map with
                                   | true -> '#'
                                   | false -> '.')
        |> System.String.Concat
        |> printfn "%3d: %s" y)

// too naive
//let map = 
//    sb 
//    |> Seq.map (fun ((sx, sy), (bx, by)) -> visible (sx, sy) (abs(bx - sx) + abs(by - sy)))
//    |> Set.unionMany

//map |> display
//Seq.filter (fun (x, y) -> y = 2000000 && not (Set.contains (x,y) beacons)) map |> Seq.length |> printfn "%A"

let manhattan (xa, ya) (xb, yb) = abs(xb - xa) + abs(yb - ya)

// try the other way
let sbd = sb |> List.map (fun ((sx, sy), (bx, by)) -> ((sx, sy), manhattan (sx, sy) (bx, by))) |> List.toArray
let dmax = sbd |> Seq.map snd |> Seq.max
let xmin = sbd |> Seq.map fst |> Seq.map fst |> Seq.min
let xmax = sbd |> Seq.map fst |> Seq.map fst |> Seq.max

let notBeacon (x,y) = 
    sbd
    |> Seq.tryFind (fun ((sx, sy), d) -> d >= manhattan (x,y) (sx, sy))
    |> fun s -> match s with
                | None -> false
                | Some(_) -> not (Set.contains (x,y) beacons)

//let yinspect=2000000
//seq {xmin-dmax..xmax+dmax}
//|> Seq.filter (fun x -> notBeacon (x, yinspect))
//|> Seq.length
//|> printfn "%A"

// faster but it turned out that wasn't my problem
//[|xmin-dmax..xmax+dmax|]
//|> Array.Parallel.choose(fun x -> cansee (x, yinspect))
//|> Seq.length
//|> printfn "%A"

// part 2
// isolate the search area as much as possible
let ymin = sbd |> Seq.map fst |> Seq.map snd |> Seq.min
let ymax = sbd |> Seq.map fst |> Seq.map snd |> Seq.max

// some dumb logic:
let range=4000000

let cansee (x,y) = sbd |> Array.exists (fun ((sx, sy), d) -> d >= manhattan (x,y) (sx, sy))
let (rmin, rmax) = (max (xmin-dmax) 0, min (xmax+dmax) range)
let blocks = 1000
let split = (rmax-rmin)/blocks


[|0..blocks|]
|> Array.Parallel.iter(fun b ->
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    for x in rmin + (b*split) .. min (rmin + ((b+1)*split)) rmax do
        for y in max (ymin-dmax) 0..min (ymax+dmax) range do
            if not (cansee (x, y)) then 
                printfn "success = %A" (x,y)
                System.Environment.Exit(0)
    printfn $"done {split} items in {timer.Elapsed} block={b}")

