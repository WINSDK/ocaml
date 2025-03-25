open !Core
open Stdio

module Lru = Lru

(*
  Watch & Reading list:
  *
  * Real world ocaml
    - https://dev.realworldocaml.org
  * Learn ocaml workshop (slightly outdated)
    - https://github.com/janestreet/learn-ocaml-workshop
  * Effective ML 2011
    - https://www.youtube.com/watch?v=4l16sYRpfL8
  * Writing a game body emulator in ocaml
    - https://linoscope.github.io/writing-a-game-boy-emulator-in-ocaml

  Write interfaces (don't just assume FP and type inference is good enough).
  It doesn't matter if you think you need it, write one.
  Make them uniform, same kinds of pattern (List.map, t.map).

  ```ocaml
  module Vec = struct
    type t = float array [@@deriving sexp]

    let copy = Array.copy
    let create0 len = Array.create ~len 0.
    let sumsq t = ...;;
    let norm t = sqrt (sumsq t)

    module Map : Core_mapjbjc
  end
  ```

  When you do write a module then, add a `type t` and use that in it's function signatures.

  When both of your match branches return different types, the compiler assumes
  you must be returning the first type. This is incorrect, it doesn't know this but it
  assumes so instead of displaying an error related to mismatching types.

  General notes about what not to do in ocaml:
  - Don't use *polymorpic variants* and definitely don't use the object system.

  To ensure your opam dependencies (similar to requirements.txt in python) match the
  current environment:
    * opam install opam-dune-lint
    * exec opam-dune-lint in project base directory


  gitignore:
  ```
  _build/
  *.opam
  ```

  Probably use one monorepo for everything:
  https://blog.janestreet.com/ironing-out-your-development-style/

  How to use flambda switch:
    * opam switch create 5.2.0+flambda2 --repos with-extensions=git+https://github.com/janestreet/opam-repository.git#with-extensions,default
    * opam switch set 5.2.0+flambda2

  ### Monorepo

    Have one switch for all projects in the monorepo:
      * opam switch create . ocaml-base-compiler
    Build all it's dependencies:
      * opam install . --deps-only
    Build + test all executables/libraries
      * dune build @all
      * dune test
    How to add new dependency
      * dependencies.opam
*)

(* Any kind of time operations using jane street's libs require this. *)
module Time = Time_float_unix
module Sys = Core.Sys
module Filename = Core.Filename;;

module Vec = struct
  type t = int array

  let to_string (x: t): string =
    let repr = x
       |> Array.map ~f:string_of_int
       |> String.concat_array ~sep:"; " in
    Printf.sprintf "int vec [| %s |]" repr

  let of_string (_s: string): t = [| 1; 2; 3 |]  (* just an example *)
end

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
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs
  in
  aux [] xs;;

let is_palindrome (xs: 'a list) = Poly.(List.rev xs = xs);;

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

let rec remove_dup = function
  | [] | [_] as l -> l (* Avoid allocation by returning original list *)
  (* (=) is an int comparison in Core, so `remove_dup` will specialize to int list.
     Therefore we use Poly.(x = y) for polymorphic comparison to keep `remove_dup` generic. *)
  | x :: (y :: _ as tl) when Poly.(x = y) -> remove_dup tl
  | x :: tl -> x :: remove_dup tl
;;

(* Version of `remove_dup` that doesn't use poly comparisons.
   This is only written like this because we use recursion. *)
let rec remove_dup2 : type a. (module Comparable.S with type t = a) -> a list -> a list =
  fun (module Compare) -> function
  | [] | [_] as l -> l
  | x :: (y :: _ as tl) when Compare.(x = y) -> remove_dup2 (module Compare) tl
  | x :: tl -> x :: remove_dup2 (module Compare) tl
;;

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
  aux xs [] (List.length xs) (* List.length takes O(n) time as it's a linked list *)
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

(* *and* makes the type checker allow accessing `aux` backwardsly *)
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

(* With the `()` this wouldn't be a function, just a value that get's resolved by the top-level *)
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

(* You must wrap every negation in a (..) due to operator precedence *)
let max_of_min3_and = Int.max (-3);;

(* f (g (h x)) == f @@ g @@ h x *)

module AllFloats = struct
  type t = {
    x: float;
    y: float;
  }
  [@@fields.no_zero_alloc] (* If your type is all floats, you may have to annotate with this *)
  [@@deriving fields ~getters, sexp]
end

module Thing = struct
  type t = {
    x: int;
    y: string;
  }
  [@@deriving fields ~getters, sexp]

  (*
    We must add a `()` as the final argument because the compiler otherwise can't figure out if
    this is a function when no parameters are passed.

    You can also just annotate t with @@deriving field ~iterators:create.
    This way you don't have to write a create function everytime.
  *)
  let create ?(x = 10) ?(y = "name") () = { x; y };;
end

let use_lru_fibonacci () =
  let module M = Lru.Make(Int) in
  let cache = M.create ~size:5 () in
  let fibonacci n =
    let rec aux n a b = if n = 0 then a else aux (n - 1) b (a + b) in
    aux n 0 1
  in
  let cached_fibonacci n =
    match M.get cache n with
    | Some result -> printf "Cache hit for n = %d: %d\n" n result; result
    | None -> 
      let result = fibonacci n in
      M.add cache ~key:n ~data:result;
      printf "Cache miss for n = %d\n" n; 
      print_s [%sexp (cache : int M.t)];
      printf "\n";
      result
  in
  List.iter [5; 10; 5; 20; 10; 25; 5] ~f:(fun n ->
      printf "Fibonacci(%d) = %d\n" n (cached_fibonacci n))
;;

let main () =
  let arr = [|1; 3; 5|] in
  (*
    Equivalent to `let _ = memset_100 arr in`.
    You can only do this when the return type is () !!!
  *)
  memset_100 arr;
  printf "%d\n" arr.(0);

  let xy : Thing.t = { x = 10; y = "lol" } in
  printf "%d %s\n" xy.x xy.y;

  let xy = Thing.create ~x:123 () in
  let xy = { xy with x = 321; } in (* Functionally update partial fields *)
  printf "%d %s\n" xy.x xy.y;

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
  time_taken |> Time.Span.to_string |> printf "Took %s to List.rev.\n";

  let rec ls_rec s =
    if Sys_unix.is_file_exn ~follow_symlinks:true s
    then [s]
    else
      Sys_unix.ls_dir s |> List.concat_map ~f:(fun sub -> ls_rec (Filename.concat s sub))
  in
  List.iter (ls_rec "./lib") ~f:print_endline;

  printf "[";
  List.iter (remove_dup [1; 2; 3; 5; 5; 9; 9]) ~f:(printf " %d");
  printf " ]\n";
  printf "[";
  List.iter (remove_dup [1.0; 2.0]) ~f:(printf " %f");
  printf " ]\n";

  (* Very verbose way of changing Options *)
  let _compute_bounds ~compare list =
    let sorted = List.sort ~compare list in
    Option.bind (List.hd sorted) ~f:(fun first ->
      Option.bind (List.last sorted) ~f:(fun last ->
        Some (first,last)))
  in
  (* Much less verbose way of doing the same thing *)
  let compute_bounds ~compare list =
    let open Option.Let_syntax in
    let sorted = List.sort ~compare list in
    let%bind first = List.hd sorted in
    let%bind last = List.last sorted in
    Some (first, last)
  in
  let lst = [5.; 100.; 2.; 100.; 30.] in
  let low, high = compute_bounds ~compare:Float.compare lst |> Option.value_exn in
  printf "low: %.02f high: %.02f\n" low high;
  (* How to debug print any expression *)
  printf !"%{sexp: Thing.t}\n%!" (Thing.create ());
  print_s [%sexp (Thing.create ()  : Thing.t)];
  printf "\n";
  use_lru_fibonacci ();

  (* Operations on a monad *)
  let x = Option.return 100 in
  let x = Option.map x ~f:(fun x -> Int.pow x 2) in
  let _x = Option.bind x ~f:(fun x -> if x < 0 then Some (-x) else None) in (* Just like rust's Option.and_then *)

  (* Operations on a monad using core's Let_syntax *)
  let _: int option =
    let open Option.Let_syntax in
    let%bind x = Some 10 in
    let%bind y = None in (* This will short circuit to the last line *)
    printf ":) %d\n" (x * y);
    None
  in
  let _: int option =
    let open Option.Let_syntax in
    let%map x = Some 10 and y = Some 20 in
    printf ":) %d\n" (x * y); (* this will now print 200 *)
    100
  in
  ()
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
    ~readme
    (Command.Param.return main)
;;
