(** Day1 **)

let input_file = "input.txt"

(* part1 *)
(* c'est HYPER lourd, je dois passer à côté de qqchose. *)
(* Franchement j'aurais dû dégainer ocamllex... quitte à faire du parsing... *)

let replace_None x y =
    if Option.is_none x then Some y else x

let get_int str i =
    (** Translates str.[i] into an int. 
        Returns -1 if invalid char. *) 
    if '0' <= str.[i] && str.[i] <= '9' then Char.(code str.[i] - code '0') else -1

let compute_line str =
    (** Computes the integer of a line *)
    let rec loop fst lst i = 
        if i >= String.length str then Option.(get fst * 10 + get lst)
        else 
            let n = get_int str i in
            if n <> -1 then loop (replace_None fst n) (Some n) (i+1)
            else loop fst lst (i+1)
    in
    loop None None 0

let () =
  let input = open_in input_file in
  let rec loop sum =
    try
        compute_line (input_line input) + sum |> loop
    with End_of_file -> sum
  in
  loop 0 |> print_int; print_newline ();
  close_in input

(* part2 *)
let numbers = [0,"zero"; 1,"one"; 2,"two"; 3,"three"; 4,"four"; 5,"five"; 6,"six"; 7,"seven"; 8,"eight"; 9,"nine"]

let try_prefix str i = 
    (** Returns Some n if the digit n is spelled out starting at str.[i].
        Returns None is no such n exists *)
    try List.find_opt (fun (x, x_str) -> String.(sub str i (length str -i) |> starts_with ~prefix:x_str)) numbers
    with Invalid_argument _ -> None (* invalid_arg can happen in String.sub near the end of str *)

let get_int str i =
    (** Translates if str.[i] into an int, including the case where it's spelled out. 
        Returns -1 if invalid. *)
    if '0' <= str.[i] && str.[i] <= '9' then Char.(code str.[i] - code '0') 
    else match try_prefix str i with
        | None -> -1
        | Some (x,_) -> x

let compute_line str =
    let rec loop fst lst i = 
        if i >= String.length str then Option.(get fst * 10 + get lst)
        else 
            let n = get_int str i in
            if n <> -1 then loop (replace_None fst n) (Some n) (i+1)
            else loop fst lst (i+1)
    in
    loop None None 0

let () =
  let input = open_in input_file in
  let rec loop sum =
    try
        compute_line (input_line input) + sum |> loop
    with End_of_file -> sum
  in
  loop 0 |> print_int; print_newline ();
  close_in input
