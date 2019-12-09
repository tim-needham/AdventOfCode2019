module Day3

open System;
open System.Diagnostics;
open System.IO;

type Direction = 
    | Up
    | Down
    | Left
    | Right;;

type Move = {
    Direction : Direction;
    Distance : int;
};;

let parse (s : string) : Move =
    match s.[0] with
    | 'U' -> { Direction = Up;      Distance = Int32.Parse (s.[1..]) };
    | 'D' -> { Direction = Down;    Distance = Int32.Parse (s.[1..]) };
    | 'L' -> { Direction = Left;    Distance = Int32.Parse (s.[1..]) };
    | 'R' -> { Direction = Right;   Distance = Int32.Parse (s.[1..]) };
    | _ -> failwithf "Unrecognised input %s" s;

let rec walk (ps : (int * int) list) ((x, y) : (int * int)) (ms : Move list) : (int * int) list =
    match ms with
    | [] -> ps;
    | m::ns when m.Direction = Up ->    let ss =    [1..m.Distance]
                                                    |> List.map (fun s -> (x, y + s))
                                        walk (ps@ss) (ss |> List.rev |> List.head) ns;
    | m::ns when m.Direction = Down ->  let ss =    [1..m.Distance]
                                                    |> List.map (fun s -> (x, y - s))
                                        walk (ps@ss) (ss |> List.rev |> List.head) ns;
    | m::ns when m.Direction = Left ->  let ss =    [1..m.Distance]
                                                    |> List.map (fun s -> (x - s, y))
                                        walk (ps@ss) (ss |> List.rev |> List.head) ns;
    | m::ns when m.Direction = Right -> let ss =    [1..m.Distance]
                                                    |> List.map (fun s -> (x + s, y))
                                        walk (ps@ss) (ss |> List.rev |> List.head) ns;

let intersections (ps : (int * int) list) (qs : (int * int) list) : (int * int) list =
    Set.intersect (Set.ofList ps) (Set.ofList qs)
    |> List.ofSeq

let manhattan (ps : (int * int) list) : int =
    ps
    |> List.map (fun (x, y) -> (Math.Abs x) + (Math.Abs y))
    |> List.sort
    |> List.head

let timing (ps : (int * int) list) (qs : (int * int) list) (is : (int * int) list) : int =
    is
    |> List.map (fun p -> (List.findIndex (fun x -> x = p) ps) + (List.findIndex (fun x -> x = p) qs) + 2)
    |> List.sort
    |> List.head

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file));

    let test =  [ "R8,U5,L5,D3"; "U7,R6,D4,L4" ]

    let wires = if testMode then test else input
                |> List.map (fun x -> x.Split([| ',' |], StringSplitOptions.None))
                |> List.map Array.toList
                |> List.map (fun x -> x |> List.map parse);

    let w1 = walk [] (0, 0) wires.[0];
    let w2 = walk [] (0, 0) wires.[1];
    let is = intersections w1 w2;

    is
    |> manhattan
    |> printfn "Day 3, part 1: %d";

    is
    |> timing w1 w2
    |> printfn "Day 3, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;