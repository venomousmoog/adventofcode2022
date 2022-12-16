let lines = System.IO.File.ReadLines "data.txt"
//let lines = System.IO.File.ReadLines "test.txt"

type Packet = PValue of int | PList of list<int>

let parse s = 
    ""

let rec packetPairs (lines:list<string>) =
    (parse(lines[0].Split(",")), parse(lines[1]))::(packetPairs (List.skip 3 lines))