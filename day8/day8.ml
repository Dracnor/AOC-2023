(** Day8 **)

let input_file = "input.txt"


(* part1 *)

let network = Hashtbl.create 10

let read_input () =
  (** Parses the input; fills the hashtbl network and returns the sequence
      of LR directions *)
  let input = open_in input_file in
  let map = input_line input in
  ignore (input_line input); (* throw blank line *)
  let rec loop () =
    try input_line input
        |> fun s -> Scanf.sscanf s "%s = (%s@, %s@)" (fun s l r -> (s, (l,r)))
        |> fun (s, (l,r)) -> Hashtbl.add network s (l,r);
        loop ()
    with End_of_file -> close_in input
  in
  loop ();
  map

let map = read_input ()

let transition index_map position =
  (** Returns the state reached from position, with direction map.[index_map] *)
  if map.[index_map] = 'L' 
  then fst (Hashtbl.find network position)
  else snd (Hashtbl.find network position)

let rec pathfinding nb_steps index_map position =
  (** Returns the number of step required to reach "ZZZ",
      starting from position and reading map.[index_map] *)
  if position = "ZZZ" then nb_steps else
  let destination = transition index_map position in 
  let next_index = (index_map +1) mod (String.length map) in
  pathfinding (nb_steps+1) next_index destination

let () = 
  pathfinding 0 0 "AAA"
  |> print_int; print_newline ()


(* part2 *)

(* No need to parse the data differently, so that global read_input that
   I previously did still works :D
   However, just running the previous algorithm on all '..A' inputs
   simunateously is too long. Instead, I'll compute the time for each '..A' 
   separatly, then least commun multiple them *)
   
let rec path_to_final_Z nb_steps index_map position =
  (** Returns the number of step required to reach a Z-ending state,
      starting from position and reading map.[index_map] *)
  if position.[2] = 'Z' then nb_steps else
  let destination = transition index_map position in 
  let next_index = (index_map +1) mod (String.length map) in
  path_to_final_Z (nb_steps+1) next_index destination

let rec gcd a b =
  (** Greatest common divisor of a and b. Euclide algorithm. *)
  if b = 0 then a else gcd b (a mod b)

let lcm a b =
  (** Least common multiple. Hopefully won't overflow. *)
  (a*b) / (gcd a b)

let () =
  Hashtbl.to_seq_keys network
  |> List.of_seq
  |> List.filter (fun s -> s.[2] = 'A')
  |> List.map (path_to_final_Z 0 0)
  |> List.fold_left lcm 1
  |> print_int; print_newline ()
