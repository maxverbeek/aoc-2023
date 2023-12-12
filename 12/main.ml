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

let rec combinations springs blocks = match (springs, blocks) with
    | ([], []) -> 1
    | ('.' :: tl, blocks) -> combinations tl blocks
    | ('#' :: tl, blocks) -> validblock (('#' :: tl), blocks)
    | ('?' :: tl, blocks) -> combinations ('#' :: tl) blocks + combinations ('.' :: tl) blocks
    | _ -> 0

and validblock = function
    | '#' :: tl, [] -> 0
    | '#' :: tl, 0 :: bs -> 0
    | cs,        0 :: bs -> combinations cs bs
    | '#' :: tl, b :: bs -> validblock (tl, ((b - 1) :: bs))
    | cs, bs -> combinations cs bs

let () =
    let lines = read_lines_from_stdin() in
    let input = lines
        |> List.map parse_line
    in

    let combinations = input |> List.map (fun (s, bs) -> combinations s bs) in
    combinations |> List.iter (Printf.printf "%d\n");
    combinations |> sum |> Printf.printf "%d\n";

