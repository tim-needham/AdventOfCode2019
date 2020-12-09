module Day2

open System;
open System.Diagnostics;
open System.IO;
open IntCode;

let rec solve (t : int) (i : int) (j : int) (is : int list) : int =
    let js =    is
                |> substitute i 1
                |> substitute j 2;
    if compute 0 [] js [] |> fst |> List.head = t then
        100 * i + j;
    else
        if i = j then
            solve t (i+1) 0 is;
        else
            solve t i (j+1) is;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file)).[0].Split([| ',' |], StringSplitOptions.None)
                |> Array.toList
                |> List.map (fun x -> Int32.Parse(x.ToString()));

    let test = [ 1; 9; 10; 3; 2; 3; 11; 0; 99; 30; 40; 50 ];

    if testMode then 
        (test, []) 
    else    (input
            |> substitute 12 1
            |> substitute 2 2, [])
    ||> compute 0 []
    |> fst
    |> List.head
    |> printfn "Day 2, part 1: %d";

    let target = 19690720;

    input
    |> solve target 0 0
    |> printfn "Day 2, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;