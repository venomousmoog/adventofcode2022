let lines = System.IO.File.ReadLines "data.txt"
//let lines = System.IO.File.ReadLines "test.txt"

open System

let valve (s:string) =
    let [|before; after|] = s.Split(';', StringSplitOptions.TrimEntries|||StringSplitOptions.RemoveEmptyEntries)
    let splat = before.Split(' ', '=') in 
    (splat[1], (int splat[5], after.Split([|' '; ','|], StringSplitOptions.TrimEntries|||StringSplitOptions.RemoveEmptyEntries)[4..]))

let valves = 
    lines 
    |> Seq.map valve
    |> Map.ofSeq

let findShortestPath s e neighbors =
    let rec bfs queue (visited:Map<'a, 'a option>) =
        let rec reconstructPath ee (visited:Map<'a, 'a option>) = 
            match visited[ee] with
            | None -> [ee]
            | Some(ee') -> ee::(reconstructPath ee' visited)

        match queue with
        | [] -> None
        | ee::_  when ee = e -> Some(List.rev (reconstructPath ee visited))
        | ee::tail -> 
            let moves = neighbors ee |> Seq.filter (fun p' -> not (Map.containsKey p' visited)) |> Seq.toList
            bfs (List.append tail moves) (moves |> Seq.fold (fun m e -> Map.add e (Some ee) m) visited)

    bfs [s] (Map.empty.Add (s, None))

let neighbor s = (snd valves[s])

let cavepaths = 
    valves.Keys
    |> Seq.map (fun k -> 
        (k, 
         valves.Keys 
         |> Seq.map (fun e -> findShortestPath k e neighbor) 
         |> Seq.choose id 
         |> Seq.map (fun e -> ((List.last e), e))
         |> Map.ofSeq))
    |> Map.ofSeq
    
type Released = 
    { 
        o:Set<string> 
        u:Set<string>
        tot:int
        tick:int
        record:list<string>
    }

    member this.TickFor path =
        let opening = (Seq.last path)
        let ticks = (Seq.length path)  // -1 for start, +1 for opening
        let flow = ticks * ((Seq.map (fun s -> (fst valves[s])) (this.o)) |> Seq.sum)
        let msg = [
            $"at {this.tick} for {ticks} ticks released {flow}"
            $"   {this.o} open"
            $"   moving to and opening {path}"
        ]
        {
            o = Set.add opening this.o
            u = Set.remove opening this.u
            tot = this.tot + flow
            tick = this.tick + ticks
            record = List.append msg this.record
         }

    static member empty = {o = Set.empty; u = Set valves.Keys; tot = 0; tick = 0; record=[] }

    static member finalTotal (r:Released) = 
        let ticks = 30 - r.tick
        let flow = ticks * ((Seq.map (fun s -> (fst valves[s])) (r.o)) |> Seq.sum)
        r.tot + flow

    member this.Finalize () = 
        let ticks = 30 - this.tick
        let flow = ticks * ((Seq.map (fun s -> (fst valves[s])) (this.o)) |> Seq.sum)
        let msg = [
            $"at {this.tick} finalizing {ticks} ticks released {flow}"
        ]
        {
            o = this.o
            u = this.u
            tot = this.tot + flow
            tick = this.tick + ticks
            record = List.append msg this.record
         }

let rec search current (released:Released):Released =
    let opt = 
        released.u 
        |> Seq.map (fun e -> cavepaths[current][e])
        |> Seq.filter (fun p -> (Seq.length p) < (30 - released.tick))
        |> Seq.filter (fun p -> (fst valves[Seq.last p]) > 0)
        |> Seq.toList

    let children = 
        opt
        |> Seq.map (fun path -> search (Seq.last path) (released.TickFor path))
        |> Seq.append [released]
        |> Seq.toList

    Seq.maxBy Released.finalTotal children

let timer = new System.Diagnostics.Stopwatch()
//timer.Start()
//search "AA" Released.empty
//|> Released.finalTotal
//|> printfn "%A"
//printfn $"elapsed = {timer.Elapsed}"

// Part 2:
// Helpful elephants!
// Turns out this is similar to above,
// but instead of tracking flow as we go along, 
// we will compute it at finalize/finalTotal only, and track
// only the time a given valve was opened.

// integer ID valves:
let inames = 
    valves
    |> Map.toSeq
    |> Seq.sortBy (fun (s, (f, n)) -> s)
    |> Seq.mapi (fun i (s, (f, n)) -> (s, i))
    |> Map.ofSeq

let ivalves = 
    valves
    |> Map.toSeq
    |> Seq.sortBy (fun (s, (f, n)) -> inames[s])
    |> Seq.mapi (fun i (s, (f, n)) -> (f, n |> Array.map (fun s -> inames[s])))
    |> Seq.toArray

let icavepaths =
    cavepaths
    |> Map.toSeq
    |> Seq.sortBy (fun (s, paths) -> inames[s])
    |> Seq.mapi (fun i (s, paths) -> 
        paths 
        |> Map.toSeq 
        |> Seq.map (fun (k, v) -> (inames[k], v |> Seq.map (fun s -> inames[s]) |> Seq.toArray))
        |> Map.ofSeq)
    |> Seq.toArray

// subset with non-zero flow?
let flowValves =
    valves
    |> Map.toSeq
    |> Seq.choose (fun (k, (f, _)) -> if f > 0 then Some(inames[k]) else None)
    |> Seq.toArray

type Released2 = 
    { 
        o:list<int * int> 
        u:Set<int>
        tick:Map<int, int>
        loc:Map<int, int>
    }

    member this.TickFor actor path =
        let opening = (Seq.last path)
        let ticks = (Seq.length path)  // -1 for start, +1 for opening
        {
            o = (opening, this.tick[actor] + ticks)::this.o
            u = Set.remove opening this.u
            tick = Map.add actor (this.tick[actor] + ticks) this.tick
            loc = Map.add actor opening this.loc
         }

    static member empty actors = { 
        o = []
        u = Set flowValves
        tick = Map.ofSeq (seq {for i in 0..actors-1 do (i, 0) })
        loc = Map.ofSeq (seq {for i in 0..actors-1 do (i, 0) })}

    static member finalTotal time (r:Released2) = 
        r.o |> Seq.map (fun (s, t) -> (fst ivalves[s]) * (time - t)) |> Seq.sum
         
let maxflow actors time = 
    let actarr = Seq.toArray (seq {0..actors-1})

    let rec bfs queue max =
        if Seq.length queue > max then 
            queue 
        else
            match queue with
            | [] -> []
            | released::rest -> 
                let opt = 
                    released.u
                    |> Seq.allPairs actarr
                    |> Seq.map (fun (a, e) -> (a, icavepaths[released.loc[a]][e]))
                    |> Seq.filter (fun (a, p) -> (Array.length p) < (time - released.tick[a]))
                    |> Seq.map (fun (a, p) -> released.TickFor a p)
                    |> Seq.toList
                bfs (List.append rest opt) max

    let rec search (released:Released2):Released2 =
        let opt = 
            released.u
            |> Seq.allPairs actarr
            |> Seq.map (fun (a, e) -> (a, icavepaths[released.loc[a]][e]))
            |> Seq.filter (fun (a, p) -> (Array.length p) < (time - released.tick[a]))

        let children = 
            opt
            |> Seq.map (fun (a, path) -> search (released.TickFor a path))
            |> Seq.append [released]

        children |> Seq.maxBy (Released2.finalTotal time)

    // search (Released2.empty actors)

    // let's gather a bunch of options from a few iterations:
    bfs [(Released2.empty actors)] 5000 
    |> Seq.toArray
    |> Array.Parallel.map (fun r -> search r)
    |> Array.maxBy (Released2.finalTotal time)

// try for 1 actor?
timer.Restart()
maxflow 1 30
|> Released2.finalTotal 30
|> printfn "%A"
printfn $"elapsed = {timer.Elapsed}"

timer.Restart()
maxflow 2 26 
|> Released2.finalTotal 26
|> printfn "%A"
printfn $"elapsed = {timer.Elapsed}"

// let's do a DFS.
// suppose, the first time I get to a node to open it, I store
// the other nodes that are open at the time and the current flow
// If that current value is higher and a superset of nodes are open
// then we should stop this search.
let maxflow2 actors time = 
    let actarr = Seq.toArray (seq {0..actors-1})

    let rec search best (released:Released2):Released2 =
        let opt = 
            released.u
            |> Seq.allPairs actarr
            |> Seq.map (fun (a, e) -> (a, icavepaths[released.loc[a]][e]))
            |> Seq.filter (fun (a, p) -> (Array.length p) < (time - released.tick[a]))
            |> Seq.filter (fun (a, p) -> 
                let lastBest = Map.tryFind (a, (Seq.last p)) best
                match lastBest with 
                | None -> true
                | Some((f, t, o)) -> 
                    if (Set.isSuperset o (Set.ofSeq released.o)) && (f > Released2.finalTotal time released) && (t < now) then
                        false
                    else
                        true

                (Array.length p) < (time - released.tick[a]))

        let children = 
            opt
            |> Seq.map (fun (a, path) -> search best (released.TickFor a path))
            |> Seq.append [released]

        children |> Seq.maxBy (Released2.finalTotal time)

    // search (Released2.empty actors)

    // let's gather a bunch of options from a few iterations:
    search Map.empty (Released2.empty actors)

