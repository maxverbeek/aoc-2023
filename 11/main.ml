let read_lines_from_stdin () =
  let rec read_lines acc =
    try
      let line = input_line stdin in
      read_lines (line :: acc)
    with
    | End_of_file -> List.rev acc
  in
  read_lines []

let find_coords ls =
    let rec find_coords' y lines = 
        match lines with
        | line :: ls -> 
            let points = line
            |> List.mapi (fun i c -> if c == '#' then Some (y, i) else Option.None)
            |> List.filter_map (fun x -> x)
            in List.append points @@ find_coords' (y+1) ls
        | [] -> []

    in
    find_coords' 0 ls

module IntSet = Set.Make(Int)

let fillgap expansion ls = 
    let set = IntSet.of_list ls in
    let max = IntSet.max_elt set in
    let gaps = List.init max (fun x -> x)
        |> List.filter (fun x -> not @@ IntSet.mem x set)
    in

    let rec addgap gaps x = match gaps with
    | [] -> x
    | gap :: tl when gap < x -> expansion + addgap tl x
    | _ :: tl -> addgap tl x
    in

    List.map (addgap gaps) ls

let expand_galaxy expansion coords =
    let ys = List.map (fun (y, _) -> y) coords |> fillgap expansion in
    let xs = List.map (fun (_, x) -> x) coords |> fillgap expansion in
    
    List.map2 (fun y x -> (y, x)) ys xs

let manhatten (a,b) (c,d) = (Int.abs (a - c)) + (Int.abs (b - d))
let sum ls = List.fold_left (+) 0 ls

let rec part1 coords = match coords with
    | [] -> 0
    | c :: tl -> (List.map (manhatten c) tl |> sum) + part1 tl

let () =
    let lines = read_lines_from_stdin() in
    let coords = lines
        |> List.map String.to_seq
        |> List.map List.of_seq
        |> find_coords
    in
    let expanded1 = expand_galaxy 1 coords in
    let expanded2 = expand_galaxy 999999 coords in
    Printf.printf "part 1: %d\n" (part1 expanded1);
    Printf.printf "part 2: %d\n" (part1 expanded2)

