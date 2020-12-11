module Day6

open System;
open System.Diagnostics;
open System.IO;

type Body = {
    Label : string;
    Satellites : Body list;
};;

let parse (s : string) : string * string =
    match s.Split( [| ')' |], StringSplitOptions.RemoveEmptyEntries) with
    | [| a; b |] -> a, b;
    | _ -> failwithf "Invalid orbital input %s" s;

let rec assemble (l : string) (os : (string * string) list) : Body =
    match os |> List.filter (fun (x, _) -> x = l) with
    | [] -> { Label = l; Satellites = [] };
    | xs -> { Label = l; Satellites = xs |> List.map (fun (_, y) -> assemble y os) };

let rec countOrbits (d : int) (b : Body) : int =
    match b.Satellites with
    | [] -> d;
    | xs -> xs
            |> List.sumBy (fun x -> (countOrbits (d+1) x))
            |> fun x -> x + d;

let rec crawlAncestors (b : Body) : string list list =
    match b.Satellites with
    | [] -> [[b.Label]];
    | xs -> xs
            |> List.map (fun x -> crawlAncestors x |> List.map (fun y -> b.Label::y))
            |> List.concat;

let rec transferLength (ps : string list) (qs : string list) : int =
    match ps, qs with
    | [], _ 
    | _, [] -> failwith "Invalid path pairing";
    | x::xs, y::ys when x <> y -> (xs |> List.length) + (ys |> List.length);
    | _::xs, _::ys -> transferLength xs ys;
                          

let commonAncestors (t1 : string) (t2 : string) (b : Body) : int =
    // assuming both Santa and I are end nodes and we can't be orbited....
    let c = crawlAncestors b

    let ps = c
            |> List.filter (fun x -> (x |> List.rev |> List.head, [t1; t2]) ||> List.contains);

    // if there are more or less than two then something went very wrong
    let ps' = ps |> List.head;
    let qs' = ps |> List.tail |> List.head;
    transferLength ps' qs';
    
let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ "COM)B";
                "B)C";
                "C)D";
                "D)E";
                "E)F";
                "B)G";
                "G)H";
                "D)I";
                "E)J";
                "J)K";
                "K)L"]
                |> List.map parse;

    let test2 = [ "COM)B";
                "B)C";
                "C)D";
                "D)E";
                "E)F";
                "B)G";
                "G)H";
                "D)I";
                "E)J";
                "J)K";
                "K)L";
                "K)YOU";
                "I)SAN" ]
                |> List.map parse;

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (parse);

    if testMode then test else input
    |> assemble "COM"
    |> countOrbits 0
    |> printfn "Day 6, part 1: %A";

    if testMode then test2 else input
    |> assemble "COM"
    |> commonAncestors "YOU" "SAN"
    |> printfn "Day 6, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;