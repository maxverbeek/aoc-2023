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

let five_of_kind hand = Array.for_all (fun c -> c = hand.cards.(0)) hand.cards

let four_of_kind hand =
  let chars = hand.cards in
  let c1 = 4 == Array.fold_left (fun acc c -> if c = chars.(0) then acc + 1 else acc) 0 chars in
  let c2 = 4 == Array.fold_left (fun acc c -> if c = chars.(1) then acc + 1 else acc) 0 chars in
  c1 || c2

let full_house hand =
  let chars = Array.copy hand.cards in
  Array.sort Char.compare chars;
  let c1 = Array.fold_left (fun acc c -> if c = chars.(0) then acc + 1 else acc) 0 chars in
  let c2 = Array.fold_left (fun acc c -> if c = chars.(4) then acc + 1 else acc) 0 chars in

  (c1 == 2 && c2 == 3) || (c1 == 3 && c2 == 2)

let three_of_kind hand =
  let chars = Array.copy hand.cards in
  Array.sort Char.compare chars;
  let c1 = Array.fold_left (fun acc c -> if c = chars.(0) then acc + 1 else acc) 0 chars in
  let c2 = Array.fold_left (fun acc c -> if c = chars.(2) then acc + 1 else acc) 0 chars in
  let c3 = Array.fold_left (fun acc c -> if c = chars.(4) then acc + 1 else acc) 0 chars in

  c1 == 3 || c2 == 3 || c3 == 3

let two_of_kind hand =
  let rec check_duplicates chars =
    match chars with
    | [] -> false
    | c :: rest ->
      if List.mem c rest then
        true
      else
        check_duplicates rest
  in check_duplicates (Array.to_list hand.cards)

let card_strength card =
  match card with
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 11
  | 'T' -> 10
  | '1' .. '9' -> int_of_char card - int_of_char '0'
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


let compare_hands h1 h2 =
  let power hand =
    let histogram =
      Array.fold_left
        (fun histogram char ->
          CharMap.update char
            (function
              | None -> Some 1
              | Some count -> Some (count + 1))
            histogram)
        CharMap.empty
        hand
    in
    match () with
    | _ when five_of_kind hand -> 5
    | _ when four_of_kind hand -> 4
    | _ when full_house hand -> 3
    | _ when three_of_kind hand -> 2
    | _ when two_of_kind hand -> 1
    | _ -> 0
  in
  let power1 = power h1 in
  let power2 = power h2 in

  if power1 == power2 then
    compare_card_by_card (Array.to_list h1.cards) (Array.to_list h2.cards)
  else
    power1 - power2


let () =
  let hands =
    read_lines_from_stdin ()
    |> List.to_seq
    |> Seq.filter_map parse_hand
    |> List.of_seq
    |> List.sort compare_hands
  in

  List.iter (fun { cards; bet } ->
    Printf.printf "Hand: Cards %c %c %c %c %c; Bet: %d\n" cards.(0) cards.(1) cards.(2) cards.(3) cards.(4) bet
  ) hands
