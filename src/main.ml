open !Core
open Stdio

(*
  General notes about what not to do in ocaml:
  - Don't use *polymorpic variants* and definitely don't use the object system.
*)

(* Any kind of time operations using jane street's libs require this. *)
module Time = Time_float_unix

let rec last xs =
  match xs with
    | [] -> None
    | [ x ] -> Some x
    | _ :: t -> last t;;

let rec last_two xs =
  match xs with
    | [] | [_] -> None
    | [ x; y] -> Some (x, y)
    | _ :: t -> last_two t;;

let rec nth xs idx =
  match xs with
    | [] -> None
    | x :: xs ->
        if idx = 0 then
          Some x
        else
          nth xs (idx - 1);;

let rec length xs =
  match xs with
    | [] -> 0
    | _ :: xs -> length xs + 1;;

let rev xs =
  let rec aux acc =
    match acc with
      | [] -> acc
      | x :: xs -> aux (x :: acc) @ xs
  in
  aux xs;;

let is_palindrome (xs: 'a list) = List.equal (=) (List.rev xs) xs;;

let encode xs =
  let rec aux xs pairs count =
    match xs with
    | [] -> pairs
    | [x] -> (1, x) :: pairs
    | x::y::xs ->
        let xs = [y] @ xs in
        if x = y then
          aux xs pairs (count + 1)
        else
          aux xs ((count, x) :: pairs) 1
  in
  List.rev (aux xs [] 1);;

type 'a rle =
  | One of 'a
  | Many of int * 'a
;;

let encode_modified xs =
  let rec aux xs pairs count =
    match xs with
    | [] -> pairs
    | [x] -> (One x) :: pairs
    | x::y::xs ->
        let xs = [y] @ xs in
        if x = y then
          aux xs pairs (count + 1)
        else
          aux xs ((Many (count, x)) :: pairs) 1
  in
  List.rev (aux xs [] 1);;

let rec duplicate xs =
  match xs with
    | [] -> []
    | x :: xs -> x :: x :: duplicate xs;;

let remove_at idx xs =
  let rec aux idx xs acc =
    match xs with
      | [] -> acc
      | x :: xs ->
          if idx = 0 then
            aux (idx - 1) xs acc
          else
            aux (idx - 1) (x :: xs) acc
  in
  aux idx xs [];;

let rec insert_at v idx xs =
  match xs with
  | [] -> [v]
  | x :: xs ->
      if idx = 0 then
        (x :: v :: xs)
      else
        x :: insert_at v (idx - 1) xs
;;

let range start ending =
  let rec aux start ending acc =
    if start > ending then
      acc
    else
      aux (start + 1) ending (start :: acc)
  in
  if start > ending then
    aux ending start []
  else
    aux start ending [] |> List.rev
;;

let rec lotto_select n m =
  if n = 0 then
    []
  else
    Random.int m :: lotto_select (n - 1) m
;;

let permutation xs =
  let rec take xs acc idx =
    match xs with
    | [] -> raise_s (String.sexp_of_t "Not found")
    | x :: xs ->
        if idx = 0 then
          (x, acc @ xs)
        else
          take xs (x :: acc) (idx - 1)
  in
  let rec aux xs acc len =
    if len = 0 then
      acc
    else
      let rand_idx = Random.int len in
      let (x, xs) = take xs [] rand_idx in
      aux xs (x :: acc) (len - 1)
  in
  aux xs [] (List.length xs)
;;

let rec gcd a b =
  if b = 0 then
    a
  else
    gcd b (a % b)
;;

let coprime a b = gcd a b = 1;;

let rec all_primes start ending =
  let is_prime n =
      let n = max n (-n) in
      let rec is_not_divisor d =
        d * d > n || (n % d <> 0 && is_not_divisor (d + 1))
      in
        is_not_divisor 2
  in
  if start > ending then
    []
  else
    let xs = all_primes (start + 1) ending in
    if is_prime start then (start :: xs) else xs
;;

type 'a node =
  | Empty
  | Node of ('a * 'a node * 'a node)
;;

let rec count_leaves node =
  match node with
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, l, r) -> (count_leaves l) + (count_leaves r)
;;

let collect_leaves node =
  let rec aux node acc =
    match node with
    | Empty -> acc
    | Node (_, Empty, Empty) -> node :: acc
    | Node (_, l, r) -> aux l (aux r acc)
  in
  aux node []
;;

(* *and* makes the type checker allow accessing `aux` backwardsly. *)
let rec at_level node lvl =
  aux node lvl []
and aux node lvl acc =
  match node with
  | Empty -> acc
  | Node (v, l, r) ->
      if lvl = 0 then
        v :: acc
      else
        let lvl = lvl - 1 in
        aux l lvl (aux r lvl acc)
;;

type 'a multiway_node =
  | Empty
  | Node of ('a * 'a multiway_node list)
;;

let rec count_nodes node =
  match node with
  | Empty -> 0
  | Node (_, children) -> List.fold children ~init:1 ~f:(fun s node -> s + (count_nodes node))
;;


let ipl node =
  match node with
  | Empty -> 0
  | Node (_, children) -> List.fold children ~init:0 ~f:(fun s node -> s + (count_nodes node) + 1)
;;

let bottom_up node =
  let rec aux node acc =
    match node with
    | Empty -> acc
    | Node (v, children) ->
        let acc = List.fold children ~init:acc ~f:(fun acc node -> acc @ (aux node acc)) in
        v::acc
  in
  aux node []
;;

type 'a graph = {
  nodes: 'a list;
  edges: ('a * 'a) list;
};;

let ex_graph: char graph =
  {nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
   edges = [('h', 'g'); ('k', 'f'); ('f', 'b'); ('f', 'c'); ('c', 'b')]};;

let mul_float x y =
  (* When using Base, the default comparison (=, <>, ..) operators work only on integers! *)
  let open Float.O in
  x * y
;;

(* With the `()` this wouldn't be a function, just a value that get's resolved by the top-level. *)
let read_flatten_stdin_all () =
  let rec aux acc =
    match In_channel.input_line In_channel.stdin with
      | Some line ->
          let line = String.chop_suffix_if_exists line ~suffix:"\n" in
          aux (line ^ acc)
      | None -> acc
  in
  aux ""
;;

let memset_100 (xs: int array) =
  let v = ref 0 in
  v := 100;
  for idx = 0 to Array.length xs - 1 do
    xs.(idx) <- !v;
  done
;;

(* You must wrap every negation in a (..) due to operator precedence. *)
let max_of_min3_and = Int.max (-3);;

(* f (g (h x)) == f @@ g @@ h x *)

let main () =
  let arr = [|1; 3; 5|] in
  (*
    Equivalent to `let _ = memset_100 arr in`.
    You can only do this when the return type is () !!!
  *)
  memset_100 arr;
  printf "%d\n" arr.(0);

  let path = "/usr/bin:/usr/local/bin:/bin:/sbin:/usr/bin" in
  String.split ~on:':' path
  |> List.dedup_and_sort ~compare:String.compare
  |> List.iter ~f:print_endline;

  let rec v acc count =
    if count > 0 then
      v (Random.int Int.max_value :: acc) (count - 1)
    else
      acc
  in
  let v = List.to_array (v [] 1234567) in
  let t = Time.now () in
  let x = Array.rev v in
  let _ = x.(Array.length x - 1) in (* Force `v` to be allocated. *)
  let time_taken = Time.diff (Time.now ()) t in
  time_taken |> Time.Span.to_string |> printf "Took %s to List.rev.\n"
;;

let readme () =
  let zone = Lazy.force Time.Zone.local in
  let today = Date.today ~zone:zone in
  let date_str = Date.to_string today in
  sprintf "Today's date is: %s.\nYou made a mistake running this." date_str
;;

let command =
  Command.basic
    ~summary:"Trying out ocaml"
    ~readme:(fun () -> readme ())
    (Command.Param.return main)
;;

let () = Command_unix.run command
