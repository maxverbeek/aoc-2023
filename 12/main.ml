let read_lines_from_stdin () =
  let rec read_lines acc =
    try
      let line = input_line stdin in
      read_lines (line :: acc)
    with
    | End_of_file -> List.rev acc
  in
  read_lines []
;;

let sum = List.fold_left ( + ) 0
let parse_nums numstr = numstr |> String.split_on_char ',' |> List.map int_of_string
let tolist str = str |> String.to_seq |> List.of_seq

let parse_line line =
  match String.split_on_char ' ' line with
  | [ springs; nums ] -> tolist springs, parse_nums nums
  | _ -> failwith "bad input"
;;

let rec repeatc n ls =
  match n with
  | 0 -> []
  | 1 -> ls
  | n -> ls @ ('?' :: repeatc (n - 1) ls)
;;

let rec repeatd n ls =
  match n with
  | 0 -> []
  | n -> ls @ repeatd (n - 1) ls
;;

type expect =
  | Blocks of int
  | End
  | Either
[@@deriving compare]

type state = expect * int list

let printexpect s =
  match s with
  | Blocks n -> Printf.sprintf "Blocks %d" n
  | Either -> Printf.sprintf "Either"
  | End -> Printf.sprintf "End"
;;

let printlist ls =
  Printf.sprintf "[%s]" (ls |> List.map string_of_int |> String.concat ", ")
;;

let step c (s : state) : state list =
  (* Printf.printf "%c -> %s %s\n" c (printexpect (fst s)) (printlist (snd s)); *)
  match c with
  | '#' ->
    (match s with
     | Blocks 1, bs -> [ End, bs ]
     | Blocks n, bs -> [ Blocks (n - 1), bs ]
     | Either, 1 :: bs -> [ End, bs ]
     | Either, b :: bs -> [ Blocks (b - 1), bs ]
     | _ -> [])
  | '.' ->
    (match s with
     | Blocks _, _ -> []
     | End, bs -> [ Either, bs ]
     | _, bs -> [ Either, bs ])
  | '?' ->
    (match s with
     (* have to consume ? as a # *)
     | Blocks 1, bs -> [ End, bs ]
     | Blocks n, bs -> [ Blocks (n - 1), bs ]
     (* have to consume ? as a . *)
     | End, bs -> [ Either, bs ]
     (* it's not clear whether ? should be a . or a #, try both *)
     | Either, 1 :: tl -> [ End, tl; Either, 1 :: tl ]
     | Either, b :: tl -> [ Blocks (b - 1), tl; Either, b :: tl ]
     | Either, [] -> [ Either, [] ])
  | _ -> failwith "invalid character"
;;

let valid_endstate = function
  | Blocks _, _ -> false
  | End, [] -> true
  | _, [] -> true
  | _ -> false
;;

module State = struct
  type t = state

  let compare (s1 : state) (s2 : state) = compare s1 s2
end

module StateMap = Map.Make (State)

let count_states (states, count) =
  let rec aux acc = function
    | [] -> acc
    | state :: rest ->
      let count =
        match StateMap.find_opt state acc with
        | Some n -> n + count
        | None -> count
      in
      aux (StateMap.add state count acc) rest
  in
  aux StateMap.empty states
;;

let merge_maps _ a b =
  match a, b with
  | Some a, Some b -> Some (a + b)
  | Some a, None -> Some a
  | None, Some b -> Some b
  | None, None -> failwith "unreachable"
;;

(* iterate over the line of characters, character by character. after each character there are a number of possible states in which we can be:
   1. expecting more block (#) characters
   2. expecting the end of a set of blocks
   3. no preference for # or .

  each of these states are then deduplicated to a histogram, before moving on to the next character.
   *)
let rec walk cs states =
  match cs with
  | c :: tl ->
    states
    |> StateMap.bindings
    (* walk a single character, and generate a new histogram of each state *)
    |> List.map (fun (s, count) -> step c s, count)
    (* the (states, count) pairs that we have here represent a list of states,
       that are repeated `count` times. At this step, duplicate states are
       merged into a single count *)
    |> List.map count_states
    (* merge all of the counted states into a single map, and continue with the next character *)
    |> List.fold_left (StateMap.merge merge_maps) StateMap.empty
    |> walk tl
  | [] ->
    (* when no more characters are left, for only the valid states, sum the number of times that state occurred *)
    StateMap.filter (fun s _count -> valid_endstate s) states
    |> StateMap.bindings
    |> List.map snd
    |> sum
;;

let startwalking cs bs =
  let init = StateMap.singleton (Either, bs) 1 in
  walk cs init
;;

let () =
  let lines = read_lines_from_stdin () in
  let input = lines |> List.map parse_line in
  let ans = List.map (fun (cs, bs) -> startwalking cs bs) input |> sum in
  Printf.printf "part 1: %d\n" ans;
  let ans2 =
    input
    |> List.map (fun (cs, bs) -> repeatc 5 cs, repeatd 5 bs)
    |> List.map (fun (cs, bs) -> startwalking cs bs)
    |> sum
  in
  Printf.printf "part 2: %d\n" ans2
;;
