open Core
include Lru_intf

module Make (K : K) = struct
  module Table = Hashtbl.Make_plain (K)

  type key = K.t

  type 'a node =
    { mutable data : 'a
    ; mutable prev : 'a node ref option
    ; mutable next : 'a node ref option
    }
  [@@deriving sexp_of, compare]

  type 'a t =
    { mapping : 'a node ref Table.t
    ; mutable head : 'a node ref option
    ; mutable tail : 'a node ref option
    ; capacity : int
    }

  let sexp_of_t sexp_of_v t =
    Hashtbl.to_alist t.mapping
    |> sexp_of_list (fun (k, node) ->
      let v = !node.data in
      Sexp.(List [ K.sexp_of_t k; sexp_of_v v ]))
  ;;

  let push_node t node =
    match t.head with
    | None ->
      t.head <- Some node;
      t.tail <- Some node
    | Some head ->
      t.head <- Some node;
      !node.prev <- Some head;
      !head.next <- Some node
  ;;

  let remove_node t node =
    if Option.exists t.head ~f:(phys_equal node) then t.head <- !node.next;
    if Option.exists t.tail ~f:(phys_equal node) then t.tail <- !node.prev;
    Option.iter !node.prev ~f:(fun prev -> !prev.next <- !node.next);
    Option.iter !node.next ~f:(fun next -> !next.prev <- !node.prev)
  ;;

  let create ~size () =
    { mapping = Table.create ~size (); head = None; tail = None; capacity = size }
  ;;

  let get t key =
    match Hashtbl.find t.mapping key with
    | Some node ->
      remove_node t node;
      push_node t node;
      Some !node.data
    | None -> None
  ;;

  let length t = Hashtbl.length t.mapping

  let add t ~key ~data =
    match t.tail, Hashtbl.length t.mapping with
    | Some tail, size when size = t.capacity ->
      remove_node t tail;
      Hashtbl.remove t.mapping key
    | _ ->
      (match Hashtbl.find t.mapping key with
       | Some node ->
         !node.data <- data;
         remove_node t node;
         push_node t node
       | None ->
         let node = ref { data; prev = None; next = None } in
         push_node t node;
         Hashtbl.add_exn t.mapping ~key ~data:node)
  ;;
end

module M = Make (struct
    type t = int [@@deriving compare, hash, sexp_of]
  end)

let%test_unit "create creates an empty cache" =
  let cache = M.create ~size:10 () in
  [%test_result: int] (M.length cache) ~expect:0;
  [%test_result: int M.node ref option] cache.head ~expect:None;
  [%test_result: int M.node ref option] cache.tail ~expect:None
;;

let%expect_test "add and get a single value" =
  let cache = M.create ~size:10 () in
  M.add cache ~key:1 ~data:"value1";
  let result = M.get cache 1 in
  print_s [%sexp (result : string option)];
  [%expect {| (value1) |}]
;;

let%expect_test "add and get multiple values" =
  let cache = M.create ~size:10 () in
  M.add cache ~key:1 ~data:"value1";
  M.add cache ~key:2 ~data:"value2";
  let result1 = M.get cache 1 in
  let result2 = M.get cache 2 in
  print_s [%sexp (result1 : string option)];
  print_s [%sexp (result2 : string option)];
  [%expect
    {|
    (value1)
    (value2)
  |}]
;;

let%expect_test "add evicts the oldest value when capacity is reached" =
  let cache = M.create ~size:2 () in
  M.add cache ~key:1 ~data:"value1";
  M.add cache ~key:2 ~data:"value2";
  M.add cache ~key:3 ~data:"value3";
  let result1 = M.get cache 1 in
  let result2 = M.get cache 2 in
  let result3 = M.get cache 3 in
  print_s [%sexp (result1 : string option)];
  print_s [%sexp (result2 : string option)];
  print_s [%sexp (result3 : string option)];
  [%expect
    {|
    (value1)
    (value2)
    ()
  |}]
;;

let%expect_test "get moves the accessed node to the head" =
  let cache = M.create ~size:3 () in
  M.add cache ~key:1 ~data:"value1";
  M.add cache ~key:2 ~data:"value2";
  M.add cache ~key:3 ~data:"value3";
  let _ = M.get cache 2 in
  let head_value = Option.map cache.head ~f:(fun h -> !h.data) in
  print_s [%sexp (head_value : string option)];
  [%expect {| (value2) |}];
  let tail_value = Option.map cache.tail ~f:(fun h -> !h.data) in
  print_s [%sexp (tail_value : string option)];
  [%expect {| (value1) |}]
;;

let%expect_test "add updates the value if the key already exists" =
  let cache = M.create ~size:3 () in
  M.add cache ~key:1 ~data:"value1";
  M.add cache ~key:1 ~data:"new_value1";
  let result = M.get cache 1 in
  print_s [%sexp (result : string option)];
  [%expect {| (new_value1) |}]
;;
