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

let parse_line line = match String.split_on_char ' ' line with
    | [springs; nums] -> (springs, (parse_nums nums))
    | _ -> failwith "bad input"

let rec combinations springs blocks = match (springs, blocks) with
    | ([], []) -> 1
    | ('.' :: tl, blocks) -> combinations tl blocks
    | ('#' :: tl, blocks) -> validblock (('#' :: tl), blocks)
    | ('?' :: tl, blocks) -> combinations ('#' :: tl) blocks + combinations ('.' :: tl) blocks
    | _ -> 0

and validblock = function
    | '#' :: tl, 0 :: bs -> 0
    | '#' :: tl, b :: bs -> validblock (tl, ((b - 1) :: bs))
    | '.' :: tl, 0 :: bs -> combinations tl bs
    | cs, bs -> combinations cs bs

let tolist str = str |> String.to_seq |> List.of_seq

let () =
    let lines = read_lines_from_stdin() in
    lines
        |> List.map parse_line
        |> List.map (fun (s, bs) -> ((tolist s), bs))
        |> List.map (fun (s, bs) -> combinations s bs)
        |> sum
        |> Printf.printf "part 1: %d";

