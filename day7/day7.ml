(** day7 **)

let input_file = "input.txt"


(* part1 *)

let cmp_card c0 c1 =
  (** Returns c0 < c1 (as an int) *)
  let rec aux = function
    | [] -> failwith "char is a not a card"
    | h::t when h = c0 -> if h = c1 then 0 else 1
    | h::t when h = c1 -> -1
    | _::t -> aux t
  in aux ['A'; 'K'; 'Q'; 'J'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2']

let cmp_hand =
  (** Compares two hands of same type *)
  List.compare cmp_card
  
let regroup l =
  (** Transforms a sorted list into the (sorted) list of the list of its identical elements 
      e.g. [a;b;b;d;e;e] becomes [[a]; [b;b]; [d]; [e;e]] *)
  let rec aux grp current = function
    | []                    -> [grp]
    | h::t when h = current -> aux (h::grp) h t 
    | h::t                  -> grp :: (aux [h] h t)
  in 
  if l = [] then [] else aux [] (List.hd l) l

(* I'll use 6 5 4...0 to reprsent Five Four Full .. High. Yeah, Five=6 and Four=5. *)

let classify hand =
  (** Classifies a hand into one of the five types *)
  let count = List.sort cmp_card hand 
          |> regroup
          |> List.map (List.length)
          |> List.sort Stdlib.compare in
  match count with
  | [5]         -> 6
  | [1;4]       -> 5
  | [2;3]       -> 4
  | [1;1;3]     -> 3
  | [1;2;2]     -> 2
  | [1;1;1;2]   -> 1
  | [1;1;1;1;1] -> 0
  | _           -> failwith "no type for hand"

let cmp_typed hand0 hand1 =
  (** Compares two hands of any types *)
  let type0, type1 = classify hand0, classify hand1 in
  if type0 <> type1 then type0-type1 else cmp_hand hand0 hand1

let winning camelCards =
   (** Computes the total winning of a game of Camel Card. Very Objective ! *)
  let ranked = List.sort (fun (hand0, _) (hand1, _) -> cmp_typed hand0 hand1) camelCards in
  let rec aux i = function
    | [] -> 0
    | (_, bid)::t -> bid*i + (aux (i+1) t)
  in aux 1 ranked
  
let list_of_str str =
  (** Converts a string into the list of its chars *)
  str |> String.to_seq |> List.of_seq

let () =
  let input = open_in input_file in
  let rec read () =
    try let str = input_line input in
        let [@warning "-8"] [s_cards; s_bid] = String.split_on_char ' ' str in
        (list_of_str s_cards, int_of_string s_bid) :: (read ())
    with End_of_file -> close_in input; []
  in
  read ()
  |> winning
  |> print_int; print_newline ()


(* part 2 *)

(* if we look at the ranks, we realize that
   it's always better to make the biggest group even bigger.
   So that what we'll do : use jokers to make the biggest group bigger *)
   
(* NB : most of these function are almost the same as before, except that now 'J'
   is the weakest card. Hence the redudancy. I could refactor it all... *)
   
   
let cmp_card_joker c0 c1 =
  (** Returns c0 < c1 (as an int) *)
  let rec aux = function
    | [] -> failwith "char is a not a card"
    | h::t when h = c0 -> if h = c1 then 0 else 1
    | h::t when h = c1 -> -1
    | _::t -> aux t
  in aux ['A'; 'K'; 'Q'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; 'J']
  
let cmp_hand_joker =
  (** Compares two hands of same type *)
  List.compare cmp_card_joker
   
let rec add_to_last x = function
  (** Adds x to the last element of a list. Creates this last element if absent. *)
  | [] -> [x]
  | [h] -> [h+x]
  | h::t -> h::(add_to_last x t)

let classify_joker hand =
  (** Classifies a hand into one of the five types. Uses jokers. *)
  let nb_jokers = List.fold_left (fun s c -> if c = 'J' then s+1 else s) 0 hand in 
  let count = List.filter (fun c -> c <> 'J') hand
          |> List.sort cmp_card_joker 
          |> regroup
          |> List.map (List.length)
          |> List.sort Stdlib.compare
          |> add_to_last nb_jokers in
  match count with
  | [5]         -> 6
  | [1;4]       -> 5
  | [2;3]       -> 4
  | [1;1;3]     -> 3
  | [1;2;2]     -> 2
  | [1;1;1;2]   -> 1
  | [1;1;1;1;1] -> 0
  | _           -> failwith "no type for hand"
  
let cmp_typed_joker hand0 hand1 =
  (** Compares two hands of any types *)
  let type0, type1 = classify_joker hand0, classify_joker hand1 in
  if type0 <> type1 then type0-type1 else cmp_hand_joker hand0 hand1

let winning_joker camelCards =
   (** Computes the total winning of a game of Camel Card. Very Objective ! *)
  let ranked = List.sort (fun (hand0, _) (hand1, _) -> cmp_typed_joker hand0 hand1) camelCards in
  let rec aux i = function
    | [] -> 0
    | (_, bid)::t -> bid*i + (aux (i+1) t)
  in aux 1 ranked

let () =
  let input = open_in input_file in
  let rec read () =
    try let str = input_line input in
        let [@warning "-8"] [s_cards; s_bid] = String.split_on_char ' ' str in
        (list_of_str s_cards, int_of_string s_bid) :: (read ())
    with End_of_file -> close_in input; []
  in
  read ()
  |> winning_joker
  |> print_int; print_newline ()
