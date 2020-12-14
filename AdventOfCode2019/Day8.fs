module Day8

open System;
open System.Diagnostics;
open System.IO;

let parse (w : int) (h : int) (s : string) : int list list list =
    s
    |> Seq.toList
    |> List.map (fun x -> Int32.Parse (x.ToString()))
    |> List.chunkBySize (w * h)
    |> List.map (fun x -> List.chunkBySize w x);

let countZeros (l : int list list) : int =
    l
    |> List.concat
    |> List.filter (fun x -> x = 0)
    |> List.length;

let fewestZeros (i : int list list list) : int =
    i
    |> List.sortBy countZeros
    |> List.head
    |> List.concat
    |> List.filter (fun x -> x = 1 || x = 2)
    |> List.partition (fun x -> x = 1)
    |> (fun (x, y) -> (List.length x) * (List.length y));

let compose (a : int list list) (b : int list list) : int list list =
    [ for j in 1..a.Length ->
        [ for i in 1..a.[j-1].Length ->
            match a.[j-1].[i-1] with
            | 0 -> 0;
            | 1 -> 1;
            | 2 -> b.[j-1].[i-1]
            | _ -> failwith "Invalid input!";
        ]
    ];

let decode (i : int list list list) : int list list =
    i
    |> List.tail
    |> List.fold (fun a x -> compose a x) (List.head i);


let prettyPrint (i : int list list) : string =
    for r in i do
        for c in r do
            printf "%c" (if c = 1 then '#' else ' ');
        printfn "";
    "";

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = "123456789012"
                |> parse 3 2;

    let test2 = "0222112222120000"
                |> parse 2 2;

    let input = Seq.toList(File.ReadLines(file))
                |> List.head
                |> parse 25 6;

    if testMode then test else input
    |> fewestZeros
    |> printfn "Day 8, part 1: %d";

    if testMode then test2 else input
    |> decode
    |> prettyPrint
    |> printfn "Day 8, part 2: %s";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;