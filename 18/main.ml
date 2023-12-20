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

type step =
  { dir : char
  ; dist : int
  }

let offsets step =
  match step with
  | 'U' -> -1, 0
  | 'D' -> 1, 0
  | 'L' -> 0, -1
  | 'R' -> 0, 1
  | _ -> failwith "bad step"
;;

let parse_line line : step =
  match String.split_on_char ' ' line with
  | [ dir; dist; color ] -> { dir = String.get dir 0; dist = int_of_string dist }
  | _ -> failwith "bad pattern"
;;

module Coord = struct
  type t =
    { row : int
    ; col : int
    }

  let compare c1 c2 =
    match compare c1.row c2.row with
    | 0 -> compare c1.col c2.col
    | c -> c
  ;;

  let rec to_seq { row; col } { dir; dist } =
    let dy, dx = offsets dir in
    let rec add n =
      match n with
      | _ when n > dist -> Seq.empty
      | _ -> Seq.cons { row = row + (dy * n); col = col + (dx * n) } (add (n + 1))
    in
    add 0
  ;;

  let ( ++ ) { row; col } { dir; dist } =
    let dy, dx = offsets dir in
    { row = row + (dist * dy); col = col + (dist * dy) }
  ;;

  let ( <-> ) c1 c2 = Int.abs (c1.row - c2.row) + Int.abs (c1.col - c2.col)
end

open Coord
module CoordSet = Set.Make (Coord)

let create_map steps =
  let rec walk coords base steps =
    match steps with
    | s :: ss -> walk (CoordSet.add_seq (Coord.to_seq base s) coords) (base ++ s) ss
    | [] -> coords
  in
  walk CoordSet.empty { row = 0; col = 0 } steps
;;

let scanline coords =
  let rec scan coords inside =
    match coords with
    | a :: b :: cs when a.row == b.row && a.col + 1 == b.col ->
      Printf.printf "row %d -> +1\n" a.row;
      1 + scan (b :: cs) inside
    | a :: b :: cs when a.row == b.row && b.col > a.col + 1 ->
      1 + (inside * (b.col - a.col)) + scan (b :: cs) (1 - inside)
    | a :: b :: cs when a.row != b.row -> 1 + scan (b :: cs) 1
    | cs -> List.length cs
  in
  scan coords 0
;;

let scanline2 coords =
  let rec scan coords inside =
    match coords with
    | a :: b :: cs when a.row == b.row && a.col + 1 == b.col ->
      Printf.printf "row %d -> +1\n" a.row;
      1 + scan (b :: cs) inside
    | a :: b :: cs when a.row == b.row && b.col > a.col + 1 ->
      1 + (inside * (b.col - a.col)) + scan (b :: cs) (1 - inside)
    | a :: b :: cs when a.row != b.row -> 1 + scan (b :: cs) 1
    | cs -> List.length cs
  in
  scan coords 0
;;

let () =
  let lines = read_lines_from_stdin () in
  let input = List.map parse_line lines in
  List.iter (fun { dir; dist } -> Printf.printf "%c %d\n" dir dist) input;
  let map = create_map input in
  let orderedcoords = List.of_seq @@ CoordSet.to_seq map in
  List.iter (fun { row; col } -> Printf.printf "row %d; col %d\n" row col) orderedcoords;
  Printf.printf "%d\n" (scanline orderedcoords)
;;
