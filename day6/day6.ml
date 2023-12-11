(** Day6 **)

let input_file = "input.txt"

(* part 1 *)

(* split time T into two : T = button + move (with T >= (button, move) >= 0)
   traveled distance td = button * move.
   td > D      <-> -button^2 + T*button - D > 0
   That's a concave 2-polynomial, so its > 0 iff button is between the two roots 
   Denote them r0 and r1 (r0 <= r1)   
   so   max(0, r0) < button < min(r1, T)
   so  floor(max(0, r0))+1 <= button <= ceil(min(r1,T)) -1
   so there are ceil(min(r1,T)) -floor(max(0, r0)) -1 solutions
*)
  
let roots a b c =
  (** Returns the two real roots of aX^2 + bX + c. Fails if complex roots *) 
  let delta = b**2. -. 4. *. a *. c in
  if delta < 0. then failwith "C is a fraud" else
  let r0, r1 = (-. b -. sqrt delta) /. (2. *. a) , (-. b +. sqrt delta) /. (2. *. a) in
  (min r0 r1, max r0 r1)
  
let nb_solutions t d =
  (** Returns the number of solution for time t and distance d *)
  let r0, r1 = roots (-. 1.) t (-. d) in
  ceil (min r1 t) -. (floor (max 0. r0)) -. 1.
  |> int_of_float

let parse_line str =
  (** Reads a part1 line *)
  String.split_on_char ':' str
  |> List.tl |> List.hd
  |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")
  |> List.map float_of_string

let () =
  let input = open_in input_file in
  let time = parse_line (input_line input) in
  let distance = parse_line (input_line input) in
  close_in input;
  List.fold_left2 (fun p t d -> p * (nb_solutions t d)) 1 time distance
  |> print_int; print_newline ()


(* part2 *)

let parse_line str =
  (** Reads a part2 line *)
  String.split_on_char ':' str
  |> List.tl |> List.hd
  |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")
  |> List.fold_left (^) ""
  |> float_of_string


let () =
  let input = open_in input_file in
  let t = parse_line (input_line input) in
  let d = parse_line (input_line input) in
  close_in input;
  nb_solutions t d
  |> print_int; print_newline ()
