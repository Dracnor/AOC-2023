(** day 4 **)

let input_file = "input.txt"


(* part 1 *)

let get_winning_have str =
  (** Parses a line to get the list of winning and the list we've got *)
  let [@warning "-8"] [winning; have] =            (* OMG SO MUCH BETTER THAN C *)
   String.split_on_char ':' str
   |> List.tl |> List.hd
   |> String.split_on_char '|'
   |> List.map (String.split_on_char ' ')
   |> List.map (List.filter (fun s -> s <> ""))
   |> List.map (List.map int_of_string)
   |> List.map (List.sort Stdlib.compare)
  in (winning, have)
  
let get_nb_matches : int list -> int list -> int=
  (** get-nb-matches winning have  is the number of matches of have into winning. 
      Assumes winning and have are sorted. *)
  let rec aux nb winning have = match (winning, have) with
    | (_, []) | ([], _)                 -> nb
    | (hw :: tw, hh :: th) when hw < hh -> aux nb tw have
    | (hw :: tw, hh :: th) when hw > hh -> aux nb winning th
    | (hw :: tw, hh :: th)              -> aux (nb+1) winning th
  in aux 0

let score_line str =
  (** Computes the score of a line *)
  let (winning, have) = get_winning_have str in
  Int.shift_left 1 (get_nb_matches winning have  -1)


let () =
  let input = open_in input_file in
  let rec loop sum =
    try
        score_line (input_line input) + sum |> loop
    with End_of_file -> sum
  in
  loop 0 |> print_int; print_newline ();
  close_in input
  
  
(* part 2 *)

let read_scrachtcards () =
  (** Builds the assoc list nb_game -> (winning, have) *)
  let input = open_in input_file in
  let rec loop i =
    try let (winning, have) = input_line input |> get_winning_have in
        (i, (winning, have)) :: (loop (i+1))
    with End_of_file -> []
  in
  let scratch = loop 0 in
  close_in input;
  scratch
  
let rec get_duplicates scratch card nb_matches =
  if   nb_matches = 0 then []
  else let dup = (card+nb_matches, List.assoc (card + nb_matches) scratch) in
       dup :: (get_duplicates scratch card (nb_matches-1))

let part2 () =
  (** Computes the number of scratchcards for part2 *)
  let scratch = read_scrachtcards () in
  let rec part2_aux total = function
    (** Computes the total number of scrachcards we'll play with.
        Its second argument is the list of scratch we still have to play *)
    | []                      -> total
    | (card, (winning, have))::t -> let nb_matches = get_nb_matches winning have in
                                 let duplicates = get_duplicates scratch card nb_matches in
                                 part2_aux (total+nb_matches) (List.rev_append duplicates t)
  in
  part2_aux (List.length scratch) scratch

let () =                                   (* Takes some time, even with sort :/ *)
  part2 () |> print_int; print_newline ()
