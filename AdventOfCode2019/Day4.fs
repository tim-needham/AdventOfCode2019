module Day4

open System;
open System.Diagnostics;
open System.IO;

let verify (x : int) : bool =
    let a, b, c, d, e, f = x / 100000, x % 100000 / 10000, x % 10000 / 1000, x % 1000 / 100, x % 100 / 10, x % 10;
    (a = b || b = c || c = d || d = e || e = f) && (a <= b && b <= c && c <= d && d <= e && e <= f);

let rec sequences (ss : (int * int) list) (d : int) (c : int) (ds : int list) : (int * int) list =
    match ds with
    | [] ->     if c > 0 then 
                    (d, c)::ss;
                else
                    ss;
    | x::xs when x = d -> sequences ss d (c+1) xs;
    | x::xs ->  sequences ((d, c)::ss) x 1 xs;

let rec equal (ds : int list) : bool =
    match ds with
    | [] -> true;
    | [_] -> true; 
    | x::y::zs ->    if x <> y then false else equal (y::zs);

let verify2 (x : int) : bool = 
    [ x % 100000 / 10000; x % 10000 / 1000; x % 1000 / 100; x % 100 / 10; x % 10 ]
    |> sequences [] (x / 100000) 1
    |> List.exists (fun (x_, y) -> y = 2)

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (fun x -> Int32.Parse(x.ToString()));

    let vs =    [input.[0]..input.[1]]
                |> List.filter verify

    vs
    |> List.length
    |> printfn "Day 4, part 1: %d";

    vs
    |> List.filter verify2
    |> List.length
    |> printfn "Day 4, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;