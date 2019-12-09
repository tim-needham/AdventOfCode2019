module Day1

open System;
open System.Diagnostics;
open System.IO;

let fuel (m : int) : int =
    (m / 3) - 2;

let rec allFuel (f : int) (m : int) : int =
    match m with
    | x when x <= 0 -> f;
    | x ->  match fuel x with
            | y when y <= 0 -> f;
            | y -> allFuel (f+y) y;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (fun x -> Int32.Parse(x.ToString()));

    // 12, 14, 1969
    let test = [ 100756 ];

    if testMode then test else input
    |> List.map fuel
    |> List.sum
    |> printfn "Day 1, part 1: %d";

    if testMode then test else input
    |> List.map (allFuel 0)
    |> List.sum
    |> printfn "Day 1, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;