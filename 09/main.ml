let read_lines_from_stdin () =
  let rec read_lines acc =
    try
      let line = input_line stdin in
      read_lines (line :: acc)
    with
    | End_of_file -> List.rev acc
  in
  read_lines []

let split_to_int x = x |> String.split_on_char ' ' |> List.map int_of_string
let last list = list |> List.rev |> List.hd

let rec sum list =
    match list with
    | [] -> 0
    | h :: t -> h + sum t

let rec derivative nums = match nums with
        | [] -> failwith "called derivative on empty list"
        | [_] -> []
        | f :: s :: tl -> (s - f) :: derivative (s :: tl)

let rec predictnext nums = 
    let d = derivative nums in
    match () with
        | _ when List.for_all (fun x -> x = 0) nums -> 0
        | _ -> last nums + predictnext d

let part1 numslist = numslist |> List.map predictnext |> sum
let part2 numslist = numslist |> List.map List.rev |> part1

let () =
    let nums = read_lines_from_stdin() |> List.map split_to_int in
    Printf.printf "part 1: %d\n" (part1 nums);
    Printf.printf "part 2: %d\n" (part2 nums)
