module Day5

open System;
open System.Diagnostics;
open System.IO;
open IntCode;

let testsPass (is : int list) (ip : int list) : int =
    let os = compute 0 [] is ip |> snd;

    if os |> List.skip 1 |> List.forall (fun x -> x = 0) then
        os |> List.head;
    else
        failwithf "Not all the tests were passed! %A" os;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [3; 0; 4; 0; 99];

    let input = Seq.toList(File.ReadLines(file)).[0].Split([| ',' |], StringSplitOptions.None)
                |> Array.toList
                |> List.map (fun x -> Int32.Parse(x.ToString()));

    ((if testMode then test else input), [1])
    ||> testsPass
    |> printfn "Day 5, part 1: %d";

    let test2 = [ 3; 21; 1008; 21; 8; 20; 1005; 20; 22; 107; 8; 21; 20; 1006; 20; 31;
                    1106; 0; 36; 98; 0; 0; 1002; 21; 125; 20; 4; 20; 1105; 1; 46; 104;
                    999; 1105; 1; 46; 1101; 1000; 1; 20; 4; 20; 1105; 1; 46; 98; 99 ];

    if testMode then
        [6..15]
        |> List.map (fun x -> (x, testsPass test2 [x]))
        |> List.fold (fun a (x, y) -> a + (string x + ": " + string y) + " ") ""
    else
        (input, [5])
        ||> testsPass
        |> (string)
    |> printfn "Day 5, part 2: %s";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;