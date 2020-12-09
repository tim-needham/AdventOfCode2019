module IntCode

let substitute (v : int) (p : int) (is : int list) : int list = 
    is |> List.mapi (fun j x -> if j = p then v else x);

let modes (m : int) : int list =
    [m % 10; (m/10) % 10; m/100];

let determine (is : int list) (m : int) (p : int) : int =
    if m = 1 then p else is.[p];

// p : int - pointer
// op : int list - outputs, functions like a stack
// is : int list - instruction set
// ip : int list - input list, funcions like a queue
// (instruction list, output list)
// parameter modes 0 = position, 1 = immediate
let rec compute (p : int) (op : int list) (is : int list) (ip : int list): int list * int list =
    let i = is.[p];
    let ms = modes (i/100);
    let det = determine is;
 
    match i % 100, is.[p+1..] with
    // add
    | 1, x :: y :: z :: _ ->    (substitute ((det ms.[0] x) + (det ms.[1] y)) z is, ip) 
                                ||> compute (p+4) op;
    // multiply
    | 2, x :: y :: z :: _ ->    (substitute ((det ms.[0] x) * (det ms.[1] y)) z is, ip)
                                ||> compute (p+4) op;
    // read input
    | 3, x :: _ ->  let h, t = List.head ip, List.tail ip;
                    (substitute h x is, t)
                    ||> compute (p+2) op;
    // emit output
    | 4, x :: _ ->  compute (p+2) (det ms.[0] x::op) is ip;
    // jump if true
    | 5, x :: y :: _ -> if (det ms.[0] x) <> 0 then
                            compute (det ms.[1] y) op is ip;
                        else
                            compute (p+3) op is ip;
    // jump if false
    | 6, x :: y :: _ -> if (det ms.[0] x) = 0 then
                            compute (det ms.[1] y) op is ip;
                        else
                            compute (p+3) op is ip;
    // less than
    | 7, x :: y :: z :: _ ->    (substitute (if (det ms.[0] x) < (det ms.[1] y) then 1 else 0) z is, ip) 
                                ||> compute (p+4) op;
    // equals
    | 8, x :: y :: z :: _ ->    (substitute (if (det ms.[0] x) = (det ms.[1] y) then 1 else 0) z is, ip) 
                                ||> compute (p+4) op;
    // halt
    | 99, _ ->  (is, op);
    | _ ->  failwithf "Error, unexpected Intcode found or ran out of input";
