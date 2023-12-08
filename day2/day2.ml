(** Day2 **)

let input_file = "input.txt"


(* part1 *)
(* Again, ocamllex is the right tool and I should use it... *)

let qty = [("red", 12); ("green", 13); ("blue", 14)] (* association list *)

let info_is_possible info =
    (** Checks if " n color" is possible. Notice the initial space. *)
    let [@warning "-8"] [_; n; color] = String.split_on_char ' ' info in
        int_of_string n <= List.assoc color qty
    
let compute_line str =
    (** Checks if a line is possible. Returns its number if it is, 0 else *)
    let [@warning "-8"] [game; draws] = String.split_on_char ':' str in
    let n_game = String.split_on_char ' ' game |> List.tl |> List.hd in (* get game nb *)
    let valid = String.split_on_char ';' draws                         (* do the actual work *)
                |> List.map (String.split_on_char ',')
                |> List.for_all (List.for_all info_is_possible)
    in
    if valid then int_of_string n_game else 0

let () =
  let input = open_in input_file in
  let rec loop sum =
    try
        compute_line (input_line input) + sum |> loop
    with End_of_file -> sum
  in
  loop 0 |> print_int; print_newline ();
  close_in input
  
  
(* part 2 *)

let rec replace_max_assoc key data = function
    (** Replaces the current association of key with data if data is greater. 
        Creates the assoc if it doesn't exist. *)
    | []       -> [(key, data)]
    | (k,d)::t -> if key = k then (key, max d data)::t else (k,d)::(replace_max_assoc key data t)


let get_rgb draw =
    (** Returns (r,g,b) the number of red green and blue drawn *)
    let update_qty qty info =
        let [@warning "-8"] [_; n; color] = String.split_on_char ' ' info in
        replace_max_assoc color (int_of_string n) qty
    in
    String.split_on_char ',' draw
    |> List.fold_left update_qty [("red", 0); ("green", 0); ("blue", 0)]
    |> fun l -> List.(assoc  "red" l, assoc "green" l, assoc "blue" l) 

let max_triple (a,b,c) (x,y,z) =
    (max a x, max b y, max c z)    

let compute_line str =
    (** Computes the minimal qty required for a line *)
    String.split_on_char ':' str
    |> List.tl |> List.hd
    |> String.split_on_char ';'
    |> List.map get_rgb
    |> List.fold_left max_triple (0, 0, 0)  

let () =
  let input = open_in input_file in
  let rec loop sum =
    try
        let (r,g,b) = compute_line (input_line input) in 
        loop (r*g*b + sum)
    with End_of_file -> sum
  in
  loop 0 |> print_int; print_newline ();
  close_in input
