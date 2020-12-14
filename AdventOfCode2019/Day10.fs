module Day10

open System;
open System.Diagnostics;
open System.IO;

let rec gcd (a : int) (b : int) : int =
    if b = 0 then
        a;
    else
        gcd b (a % b);

let vectors (w : int) (h : int) ((fx, fy) : int * int) : (int * int) list =
    List.allPairs [0..w] [0..h]
    |> List.filter (fun (x, y) -> x <> fx || y <> fy)
    |> List.map (fun (x, y) -> (x - fx, y - fy))
    |> List.map (fun (x, y) ->  let d = Math.Abs(gcd x y);
                                (x/d, y/d))
    |> List.distinct;

let rec travel (w : int) (h : int) ((fx, fy) : int * int) (ms : char list list) ((dx, dy) : int * int) : bool =
    let fx' = fx + dx;
    let fy' = fy + dy;

    if fx' < 0 || fy' < 0 || fx' > w || fy' > h then
        false
    else    
        match ms.[fy'].[fx'] with
        | '.' -> travel w h (fx', fy') ms (dx, dy);
        | '#' -> true;
        | x -> failwithf "Invalid symbol in grid %c" x;
        
let scan (ms : char list list) : (int * int * int) list =
    [ for j in 1..ms.Length ->
        [ for i in 1..ms.[j-1].Length ->
            match ms.[j-1].[i-1] with
            | '.' -> (i-1, j-1, 0);
            // draw a vector from the asteroid to each square on the grid
            // walk that vector to see if it gets blocked by an asteroid
            // count the blocked vectors
            | '#' ->    let w, h = (ms.[0].Length - 1), (ms.Length - 1);
                        (i-1, j-1) 
                        |> vectors w h
                        |> List.map (fun x -> travel w h (i-1, j-1) ms x)
                        |> List.filter (fun x -> x)
                        |> List.length
                        |> (fun x -> (i-1, j-1, x))
            | _ -> failwith "Invalid input!";
        ]
    ]
    |> List.concat;

let bestLocation (ms : char list list) : int * int * int =
    scan ms
    |> List.sortByDescending (fun (_, _, a) -> a)
    |> List.head;

let asteroids (ms : char list list) : (int * int) list =
    seq {
        for j in 1..ms.Length do 
            for i in 1..ms.[j-1].Length do
                match ms.[j-1].[i-1] with
                | '.' -> ();
                | '#' -> yield (i-1, j-1);
                | _ -> failwith "Invalid input!";
    }
    |> Seq.toList;

let manhattan ((fx, fy) : int * int) ((x, y) : int * int) : int =
    Math.Abs(x-fx) + Math.Abs(y-fy);

let rec fireLaser (gs : (float * (int * int) list) list) : (int * int) list =
    match gs with
    | [] -> [];
    | (a, xs)::ys ->    let p, qs = List.head xs, List.tail xs;
                        match qs with
                        | [] -> p::(fireLaser ys);
                        | _ -> p::(fireLaser (ys@[(a, qs)]));

let giantLaser (ms : char list list) =
    let (bx, by) = bestLocation ms |> (fun (x, y, _) -> (x, y));

    // get all the asteroids
    // sort the asteroids by distance, manhattan *should* work
    // drop the "best location" asteroid from the results
    // sort by angle using atan2 but correct for direction and offset
    // group and sort by corrected atan2 value but drop from the result
    // fire the laser until all asteroids are hit
    // return the 200th element in the sequence
    ms
    |> asteroids
    |> List.sortBy (fun (x, y) -> manhattan (bx, by) (x, y))
    |> List.tail
    |> List.map (fun (x, y) -> (x, y, ((5.0 * Math.PI / 2.0) - Math.Atan2(float (by - y), float (x - bx))) % (Math.PI * 2.0)))
    |> List.groupBy (fun (_, _, a) -> a)
    |> List.map (fun (a, xs) -> (a, xs |> List.map (fun (x, y, _) -> (x, y))))
    |> List.sortBy (fun (a, _) -> a)
    |> fireLaser
    |> List.skip 199
    |> List.head

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ ".#..##.###...#######";
                "##.############..##.";
                ".#.######.########.#";
                ".###.#######.####.#.";
                "#####.##.#.##.###.##";
                "..#####..#.#########";
                "####################";
                "#.####....###.#.#.##";
                "##.#################";
                "#####.##.###..####..";
                "..######..##.#######";
                "####.##.####...##..#";
                ".#####..#.######.###";
                "##...#.##########...";
                "#.##########.#######";
                ".####.#.###.###.#.##";
                "....##.##.###..#####";
                ".#.#.###########.###";
                "#.#.#.#####.####.###";
                "###.##.####.##.#..##" ]
                |> List.map Seq.toList;

    let input = Seq.toList(File.ReadLines(file))
                |> List.map Seq.toList;

    if testMode then test else input
    |> bestLocation
    |> (fun (_, _, a) -> a)
    |> printfn "Day 10, part 1: %d";

    if testMode then test else input
    |> giantLaser
    |> (fun (x, y) -> (100 * x) + y)
    |> printfn "Day 10, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;