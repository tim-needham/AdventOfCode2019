module Day12

open System;
open System.Diagnostics;
open System.IO;

type Point3 = {
    X : int;
    Y : int;
    Z : int;
};;

type Satellite = {
    Position : Point3;
    Velocity : Point3;
};;

let parse (s : string) : Satellite =
    match s.Split([| ','; ' '; '='; '>' |], StringSplitOptions.RemoveEmptyEntries) with
    | [| _; x; _; y; _; z |] -> { Position = { X = Int32.Parse(x); Y = Int32.Parse(y); Z = Int32.Parse(z) }; Velocity = { X = 0; Y = 0; Z = 0 } };
    | _ -> failwithf "Unrecognized input %s" s;

let add (a : Point3) (b : Point3) : Point3 =
    { X = a.X + b.X; Y = a.Y + b.Y; Z = a.Z + b.Z };
    

let gravity (a : Satellite) (b : Satellite) : Point3 =
    let x = if a.Position.X < b.Position.X then 1 elif a.Position.X > b.Position.X then -1 else 0;
    let y = if a.Position.Y < b.Position.Y then 1 elif a.Position.Y > b.Position.Y then -1 else 0;
    let z = if a.Position.Z < b.Position.Z then 1 elif a.Position.Z > b.Position.Z then -1 else 0;
    { X = x; Y = y; Z = z };

let velocity (a : Satellite) (bs : Satellite list) : Point3 =
    bs
    |> List.fold (fun v b ->    let v' = gravity a b;
                                { X = v.X + v'.X; Y = v.Y + v'.Y; Z = v.Z + v'.Z }) a.Velocity;

let moveStep (ss : Satellite list) : Satellite list =
    ss
    |> List.map (fun s ->   let ts = List.except [s] ss;
                            let v = velocity s ts;
                            { Position = add s.Position v; Velocity = v } );

let rec move (n : int) (ss : Satellite list) : Satellite list =
    match n with
    | 0 -> ss;
    | x ->  ss
            |> moveStep
            |> move (x-1);

let energy (s : Satellite) : int =
    let pot = Math.Abs(s.Position.X) + Math.Abs(s.Position.Y) + Math.Abs(s.Position.Z);
    let kin = Math.Abs(s.Velocity.X) + Math.Abs(s.Velocity.Y) + Math.Abs(s.Velocity.Z);
    pot * kin

let rec gcd (a : int64) (b : int64) : int64 =
    if b = 0L then
        a;
    else
        gcd b (a % b);

let lcm (a : int64) (b : int64) : int64 =
    a * b / gcd a b;

let rec findPeriods (c : int64) (x : int64) (y : int64) (z : int64) (ss : Satellite list) (ts : Satellite list) : int64 =
    let c' = c + 1L;
    let ts' = moveStep ts;
    let us = List.zip ss ts';
    let x' = if x <> 0L then x elif us |> List.forall (fun (a, b) -> a.Position.X = b.Position.X && a.Velocity.X = b.Velocity.X) then c' else 0L; 
    let y' = if y <> 0L then y elif us |> List.forall (fun (a, b) -> a.Position.Y = b.Position.Y && a.Velocity.Y = b.Velocity.Y) then c' else 0L; 
    let z' = if z <> 0L then z elif us |> List.forall (fun (a, b) -> a.Position.Z = b.Position.Z && a.Velocity.Z = b.Velocity.Z) then c' else 0L; 

    if x' <> 0L && y' <> 0L && z' <> 0L then
        lcm x' (lcm y' z');
    else
        findPeriods c' x' y' z' ss ts';
    
let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ "<x=-1, y=0, z=2>";
                "<x=2, y=-10, z=-7>";
                "<x=4, y=-8, z=8>";
                "<x=3, y=5, z=-1>" ]
                |> List.map parse;

    let input = Seq.toList(File.ReadLines(file))
                |> List.map parse;

    if testMode then (10, test) else (1000, input)
    ||> move
    |> List.sumBy energy
    |> printfn "Day 12, part 1: %d";

    if testMode then (test, test) else (input, input)
    ||> findPeriods 0L 0L 0L 0L
    |> printfn "Day 12, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;