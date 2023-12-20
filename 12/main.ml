let read_lines_from_stdin () =
  let rec read_lines acc =
    try
      let line = input_line stdin in
      read_lines (line :: acc)
    with
    | End_of_file -> List.rev acc
  in
  read_lines []

let sum = List.fold_left (+) 0

let parse_nums numstr = numstr |> String.split_on_char ',' |> List.map int_of_string


let tolist str = str |> String.to_seq |> List.of_seq

let parse_line line = match String.split_on_char ' ' line with
    | [springs; nums] -> ((tolist springs), (parse_nums nums))
    | _ -> failwith "bad input"

let dprint title springs blocks =
    Printf.printf "%s: " title;
    List.iter (Printf.printf "%c") springs;
    Printf.printf " [";
    List.iter (Printf.printf "%d, ") blocks;
    Printf.printf "]\n"

let memo f =
    let h = Hashtbl.create 100000 in
    let rec g x =
        match Hashtbl.find_opt h x with
        | Some res -> res
        | None ->
            let res = f g x in
            Hashtbl.add h x res;
            res
    in
    g

let combinations springs blocks =
  let memo_table = Hashtbl.create 100 in

  let rec combinations_memo springs blocks =
    let key = (springs, blocks) in
    match Hashtbl.find_opt memo_table key with
    | Some result -> result
    | None ->
      let result =
        match (springs, blocks) with
        | ([], []) -> 1
        | ('.' :: tl, blocks) -> combinations_memo tl blocks
        | ('#' :: tl, []) -> 0
        | ('#' :: tl, b :: blocks) -> validblock (('#' :: tl), (b :: blocks))
        | ('?' :: tl, blocks) ->
            let c = combinations_memo ('.' :: tl) blocks + combinations_memo ('#' :: tl) blocks in
            c
        | _ -> 0
      in
      Hashtbl.add memo_table key result;
      result

  and validblock (cs, bs) =
    match (cs, bs) with
    | '#' :: tl, b :: bs when b > 0 -> validblock (tl, ((b - 1) :: bs))
    | '?' :: tl, b :: bs when b > 0 -> validblock (tl, ((b - 1) :: bs))
    | c :: cs, 0 :: bs when c != '#' -> combinations_memo cs bs
    | [], 0 :: bs -> combinations_memo [] bs
    | _ -> 0
  in

  combinations_memo springs blocks
let rec repeatc n ls = match n with
    | 0 -> []
    | 1 -> ls
    | n -> ls @ '?' :: repeatc (n-1) ls

let rec repeatd n ls = match n with
    | 0 -> []
    | n -> ls @ repeatd (n-1) ls

let rec take_while p ls = match ls with
    | hd :: tl when p hd -> hd :: take_while p tl
    | _ -> []

let rec drop_while p ls = match ls with
    | hd :: tl when not @@ p hd -> drop_while p tl
    | tl -> tl

let rec combi springs blocks =
    let fstregion = take_while (fun x -> x != '.') springs in
    let lstregion = drop_while (fun x -> x != '.') springs |> drop_while (fun x -> x == '.') in
    let matches = fit fstregion blocks in
    (* also check if we dont attempt to fit any block here *)
    let skipped = combi lstregion blocks in
    List.fold_left (fun acc (p, bs) -> acc + p * (combi lstregion bs)) skipped matches

(* fit returns a list of tuples: (combinations, blocks remaining when using these combinations) *)
(* cs is guaranteed not to contain . (so only # and ?) *)
(* example: if we need to fit [1, 1] in ???, we can do so #.#; [] but we can also do it ..#; [1] *)
(* so the result here is [ (1, []); (1, [1])] *)
(* example 2: ????? [1,1,1,2] -> [ (5, [1,1,2]); (6, [1,2]); (1, [2]) ] *)
(* example 3: ##### [1,1,1,2] -> [] because we cannot fit 1. there has to be a gap *)
and fit cs bs = 
    let l = List.length cs in
    match bs with
    (* when b > l it does not fit, there are 0 ways *)
    | b :: bs when b > l -> []
    (* when b == l, there is exactly one way to fit this block, and then we're out of space *)
    | b :: bs when b = l -> [ (1, bs) ]
    (* when b < l and we don't fit the next block, there are b - l ways where we can put it *)
    | b :: bs when b < l -> [ ((b - l), bs) ]
    (* when we can fit more than 1, we have to leave room: *)
    | b :: n :: bs when (b + n + 1) <= l ->
        let count = howmanyfit l (b :: n :: bs) in []

and howmanyfit l bs = match bs with
    | b :: n :: bs when (b + n) < l -> 1 + howmanyfit (b - l - 1) (n :: bs)
    | b :: bs when b < l -> 1
    | _ -> 0

let solve ls = 
    let cs = List.map (fun (s, bs) -> combinations s bs) ls in
    List.iter (Printf.printf "%d\n") cs;
    sum cs

type expect = Blocks of int | End | Either
type state = expect * int list

let step c (s: state): state list = match c with
    | '#' -> begin match s with
        | Blocks n, bs when n > 1 -> [(Blocks (n-1), bs)]
        | Blocks 1, bs -> [End, bs]
        | End, _ -> []
        | _, bs -> [(Either, bs)]
        end

    | '.' -> begin match s with
        | Blocks _, _ -> []
        | End, bs -> [(Either, bs)]
        | _, bs -> [(Either, bs)]
        end

    | '?' -> begin match s with
        (* consume ? as a # *)
        | Blocks 1, bs -> [(End, bs)]
        | Blocks n, bs -> [(Blocks (n-1), bs)]

        (* consume ? as a . *)
        | End, bs -> [(Either, bs)]

        (* it's not clear whether ? should be a . or a #, try both *)
        | Either, b :: tl -> [(Blocks (b-1), tl); (Either, b :: tl)]
        | Either, [] -> [(Either, [])]
        end
    | _ -> failwith "invalid character"

let valid_endstate = function
    | Blocks _, _ -> false
    | End, [] -> true
    | _, [] -> true
    | _ -> false

let rec walk cs states = match cs with
    | c :: tl -> states |> List.map (step c) |> List.flatten |> walk tl
    | [] -> List.filter valid_endstate states |> List.length

let startwalking cs bs =
    let init = [(Either, bs)] in
    walk cs init

let () =
    let lines = read_lines_from_stdin() in
    let input = lines |> List.map parse_line in
    
    let ans = List.map (fun (cs, bs) -> startwalking cs bs) input |> sum in
    Printf.printf "ans: %d\n" ans;
