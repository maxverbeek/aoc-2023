open Printf
open Stdlib

module CharMap = Map.Make(Char)

type hand = {
  cards: char array;
  bet: int;
}


let read_lines_from_stdin () =
  let rec read_lines acc =
    try
      let line = input_line stdin in
      read_lines (line :: acc)
    with
    | End_of_file -> List.rev acc
  in
  read_lines []

let parse_hand line =
  let input_channel = Scanf.Scanning.from_string line in

  try
    let h = Scanf.bscanf input_channel "%c%c%c%c%c %d" (fun c1 c2 c3 c4 c5 bet -> { cards = [|c1; c2; c3; c4; c5|]; bet = bet; }) in
    Some(h)
  with
  | End_of_file _ ->
    None

let five_of_kind maphand = CharMap.exists (fun _ count -> count = 5) maphand
let four_of_kind maphand = CharMap.exists (fun _ count -> count = 4) maphand
let three_of_kind maphand = CharMap.exists (fun _ count -> count = 3) maphand
let two_of_kind maphand = CharMap.exists (fun _ count -> count = 2) maphand
let full_house maphand = (three_of_kind maphand) && (two_of_kind maphand)
let two_pairs maphand =
  let bindings = CharMap.bindings maphand in
  let pairs = Seq.filter (fun (c, count) -> count = 2) (List.to_seq bindings) in
  Seq.length pairs = 2



let card_strength card =
  match card with
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 11
  | 'T' -> 10
  | '1' .. '9' -> (int_of_char card) - (int_of_char '0')
  | _ -> failwith "Invalid card"

let rec compare_card_by_card cards1 cards2 =
  match cards1, cards2 with
  | [], [] -> 0
  | c1 :: rest1, c2 :: rest2 ->
    let s1 = card_strength c1 in
    let s2 = card_strength c2 in
    if s1 == s2 then
      compare_card_by_card rest1 rest2
    else
      s1 - s2

let histogram hand =
  let rec count_helper chars map =
    match chars with
    | [] -> map
    | c :: rest ->
      let updated_map =
        match CharMap.find_opt c map with
        | None -> CharMap.add c 1 map
        | Some count -> CharMap.add c (count + 1) map
      in count_helper rest updated_map
    in count_helper hand CharMap.empty

let compare_hands h1 h2 =
  let power hand =
    let map = histogram (Array.to_list (Array.copy hand)) in
    match () with
    | _ when five_of_kind map -> 6
    | _ when four_of_kind map -> 5
    | _ when full_house map -> 4
    | _ when three_of_kind map -> 3
    | _ when two_pairs map -> 2
    | _ when two_of_kind map -> 1
    | _ -> 0
  in
  let power1 = power h1.cards in
  let power2 = power h2.cards in

  if power1 == power2 then
    compare_card_by_card (Array.to_list h1.cards) (Array.to_list h2.cards)
  else
    power1 - power2

let rec ranksum rank cards =
  match cards with
  | [] -> 0
  | c :: tail -> (c.bet * rank) + ranksum (rank + 1) tail

let () =
  let hands =
    read_lines_from_stdin ()
    |> List.to_seq
    |> Seq.filter_map parse_hand
    |> List.of_seq
    |> List.sort compare_hands
  in
  Printf.printf "part 1: %d\n" (ranksum 1 hands)
  (* List.iter (fun { cards; bet } -> *)
  (*   Printf.printf "Hand: Cards %c %c %c %c %c; Bet: %d\n" cards.(0) cards.(1) cards.(2) cards.(3) cards.(4) bet *)
  (* ) hands *)
