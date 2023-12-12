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

let rec combinations springs blocks =
    (* dprint "combinations" springs blocks; *)
    match (springs, blocks) with
    | ([], []) -> 1
    | ('.' :: tl, blocks) -> combinations tl blocks
    | ('#' :: tl, []) -> 0
    | ('#' :: tl, b :: blocks) -> validblock (('#' :: tl), (b :: blocks))
    | ('?' :: tl, blocks) ->
        let c = combinations ('.' :: tl) blocks + combinations ('#' :: tl) blocks in
        c
    | _ -> 0

and validblock (cs, bs) =
    (* dprint "validblocks" cs bs; *)
    match (cs, bs) with
    | '#' :: tl, b :: bs when b > 0 -> validblock (tl, ((b - 1) :: bs))
    | '?' :: tl, b :: bs when b > 0 -> validblock (tl, ((b - 1) :: bs))
    (* the only way to escape this loop succesfully is to end up with 0 left *)
    | c :: cs, 0 :: bs when c != '#' -> combinations cs bs
    | [],      0 :: bs -> combinations [] bs
    (* all other cases are unsuccessfull *)
    | _ -> 0

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

let () =
    let lines = read_lines_from_stdin() in
    let input = lines
        |> List.map parse_line
    in

    (* input |> solve |> Printf.printf "part 1: %d\n"; *)

    let input2 = List.map (fun (s, bs) -> (repeatc 5 s, repeatd 5 bs)) input in
    input2 |> List.iter (fun (s, bs) -> dprint "input" s bs);

    input2 |> solve |> print_int
