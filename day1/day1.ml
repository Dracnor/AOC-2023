(** Day1 **)

let input_file = "input.txt"

(* part1 *)
(* c'est HYPER lourd, je dois passer à côté de qqchose. *)
(* Franchement j'aurais dûd égainer ocamllex... quitte à faire du parsing... *)

let compute_line str =
    (** Computes the integer of a line *)
    let maj_fst fst fst_new =
        if Option.is_none fst then Some fst_new else fst
    in
    let get_int i = 
        if '0' <= str.[i] && str.[i] <= '9' then Char.(code str.[i] - code '0') else -1
    in
    let rec loop fst lst i = 
        if i >= String.length str then Option.(get fst * 10 + get lst)
        else 
            let n = get_int i in
            if n <> -1 then loop (maj_fst fst n) (Some n) (i+1)
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

let compute_line str =
    let maj_fst fst fst_new =
        if Option.is_none fst then Some fst_new else fst
    in
    let try_prefix i = 
        (** Checks if a written number starts at index i *)
        try List.find_opt (fun (x, x_str) -> String.(sub str i (length str -i) |> starts_with ~prefix:x_str)) numbers
        with Invalid_argument _ -> None
    in
    let get_int i =
        (** Each index either is an int, either starts a written number, either is nothing *)
        if '0' <= str.[i] && str.[i] <= '9' then Char.(code str.[i] - code '0') else 
        match try_prefix i with
            | None -> -1
            | Some (x,_) -> x
    in
    let rec loop fst lst i = 
        if i >= String.length str then Option.(get fst * 10 + get lst)
        else 
            let n = get_int i in
            if n <> -1 then loop (maj_fst fst n) (Some n) (i+1)
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
