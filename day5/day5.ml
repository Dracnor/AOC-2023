(** day 5 **)

let input_file = "input.txt"


(* part 1 *)

let get_one_map input =
  (** Read the lines of a map to get (source, (destination, list_of_maps)).
      Assumes input is opened *)
  let [@warning "-8"] [src; _; dst] =
    input_line input
    |> String.split_on_char ' '
    |> List.hd
    |> String.split_on_char '-'
  in
  let rec read_map () =
    try
      let str = input_line input in if str = "" then [] else
      let [@warning "-8"] [dst_range; src_range; len] =
        String.split_on_char ' ' str
        |> List.map int_of_string
      in (dst_range, src_range, len):: (read_map () )
    with End_of_file -> []
  in
  (src, (dst, read_map ()) )

let rec get_all_maps input =
  (** Reads the input to get all of the maps. Assumes opened. *)
  try
    let m = get_one_map input in
    m :: (get_all_maps input)
  with End_of_file -> []

let rec transform number = function
  (** Given a number and the map of ranges used to transform it, returns its transformation.
      i.e. does one step, e.g. fertilizer->water *)
  | [] -> number
  | (dst_range, src_range, range) :: t -> if src_range <= number 
                                             && number < src_range + range
                                          then dst_range + (number - src_range)
                                          else transform number t

let rec get_location all_maps source number =
  (** Get the location we get starting from number, of category source *) 
  if source = "location" then number
  else let (destination, list_of_maps) = List.assoc source all_maps in
       get_location all_maps destination (transform number list_of_maps)

let () =
  let input = open_in input_file in
  let seeds = input_line input
              |> String.split_on_char ' '
              |> List.tl
              |> List.map int_of_string
  in
  ignore (input_line input);         (* throw first empty line *)
  let all_maps = get_all_maps input in
  close_in input;
  List.map (get_location all_maps "seed") seeds
  |> List.sort Stdlib.compare
  |> List.hd
  |> print_int; print_newline ()


(* Part2. *)
(* I reverse bruteforce. It works, and it's easy. *)

let rec get_reverse_map = function
  (** Reverses the list of maps to get a location -> ... -> seed *)
  | [] -> []
  | (source, (destination, mapping))::t -> (destination, (source, mapping)) :: (get_reverse_map t)
   
let rec reverse_transform number = function
  (** Checks how number can have been obtained from a given transformaion map *)
  | [] -> number
  | (dst_range, src_range, range)::t -> (* can number have been created by this rule ? *)
       let offset = number - dst_range in
       if offset < 0 || offset > range
       then reverse_transform number t
       else src_range + offset
   
   
let rec get_seed all_maps current_state number =
  (** Flows back the transformations to get  the original seed *)
  if current_state = "seed" then number
  else 
    let (source, mapping) = List.assoc current_state all_maps in
    get_seed all_maps source (reverse_transform number mapping) 
    

let rec is_valid s = function
  (** Checks if s is a seed *)
  | []             -> false
  | [h]            -> failwith "Odd number of integers in seeds"
  | seed::range::t -> (seed <= s && s < seed+range) || is_valid s t


let rec find_first_valid all_maps i seeds =
  (** Goes through the integers until finding a valid one, i.e. one that can come from a seed *)
  if is_valid (get_seed all_maps "location" i) seeds then i
  else find_first_valid all_maps (i+1) seeds


let () =
  let input = open_in input_file in
  let seeds = input_line input
              |> String.split_on_char ' '
              |> List.tl
              |> List.map int_of_string
  in
  ignore (input_line input);         (* throw first empty line *)
  let all_maps = get_all_maps input in
  close_in input;
  find_first_valid (get_reverse_map all_maps) 0 seeds
  |> print_int; print_newline ()
