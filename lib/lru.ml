open Core

(* Key must implement compare, sexp, hash *)
(* Value must implement compare *)
type 'v node =
  { mutable value : 'v
  ; mutable prev : 'v node option
  ; mutable next : 'v node option
  }
[@@deriving equal, compare, sexp]

type ('k, 'v) t =
  { mapping : ('k, 'v node) Hashtbl.t
  ; mutable head : 'v node option
  ; mutable tail : 'v node option
  ; equal : 'v -> 'v -> bool
  ; capacity : int
  }

let push_node t node =
  match t.head with
  | None ->
    t.head <- Some node;
    t.tail <- Some node
  | Some head ->
    t.head <- Some node;
    node.prev <- Some head;
    head.next <- Some node
;;

let remove_node t node =
  if Option.exists t.head ~f:(fun head -> equal_node t.equal node head)
  then t.head <- node.next;
  if Option.exists t.tail ~f:(fun tail -> equal_node t.equal node tail)
  then t.tail <- node.prev;
  Option.iter node.prev ~f:(fun prev -> prev.next <- node.next);
  Option.iter node.next ~f:(fun next -> next.prev <- node.prev)
;;

let create ~size m_key m_value =
  let compare = (Base.Hashable.of_key m_value).compare in
  let equal x y = compare x y = 0 in
  { mapping = Hashtbl.create ~size m_key
  ; head = None
  ; tail = None
  ; equal
  ; capacity = size
  }
;;

let get t key =
  match Hashtbl.find t.mapping key with
  | Some node ->
    remove_node t node;
    push_node t node;
    Some node.value
  | None -> None
;;

let put (t : ('k, 'v) t) key value =
  match t.tail, Hashtbl.length t.mapping with
  | Some tail, size when size = t.capacity ->
    remove_node t tail;
    Hashtbl.remove t.mapping key
  | _ ->
    (match Hashtbl.find t.mapping key with
     | Some node ->
       node.value <- value;
       remove_node t node;
       push_node t node
     | None ->
       let node = { value; prev = None; next = None } in
       push_node t node;
       Hashtbl.add_exn t.mapping ~key ~data:node)
;;

let length t = Hashtbl.length t.mapping

let%test_unit "create creates an empty cache" =
  let cache = create ~size:10 (module Int) (module Int) in
  [%test_result: int] (Hashtbl.length cache.mapping) ~expect:0;
  [%test_result: int node option] cache.head ~expect:None;
  [%test_result: int node option] cache.tail ~expect:None
;;

let%expect_test "put and get a single value" =
  let cache = create ~size:10 (module Int) (module String) in
  put cache 1 "value1";
  let result = get cache 1 in
  print_s [%sexp (result : string option)];
  [%expect {| (value1) |}]
;;

let%expect_test "put and get multiple values" =
  let cache = create ~size:10 (module Int) (module String) in
  put cache 1 "value1";
  put cache 2 "value2";
  let result1 = get cache 1 in
  let result2 = get cache 2 in
  print_s [%sexp (result1 : string option)];
  print_s [%sexp (result2 : string option)];
  [%expect {|
    (value1)
    (value2)
  |}]
;;

let%expect_test "put evicts the oldest value when capacity is reached" =
  let cache = create ~size:2 (module Int) (module String) in
  put cache 1 "value1";
  put cache 2 "value2";
  put cache 3 "value3";
  let result1 = get cache 1 in
  let result2 = get cache 2 in
  let result3 = get cache 3 in
  print_s [%sexp (result1 : string option)];
  print_s [%sexp (result2 : string option)];
  print_s [%sexp (result3 : string option)];
  [%expect {|
    (value1)
    (value2)
    ()
  |}]
;;

let%expect_test "get moves the accessed node to the head" =
  let cache = create ~size:3 (module Int) (module String) in
  put cache 1 "value1";
  put cache 2 "value2";
  put cache 3 "value3";
  let _ = get cache 2 in
  let head_value = Option.map cache.head ~f:(fun h -> h.value) in
  print_s [%sexp (head_value : string option)];
  [%expect {| (value2) |}];
  let tail_value = Option.map cache.tail ~f:(fun h -> h.value) in
  print_s [%sexp (tail_value : string option)];
  [%expect {| (value1) |}]
;;

let%expect_test "put updates the value if the key already exists" =
  let cache = create ~size:3 (module Int) (module String) in
  put cache 1 "value1";
  put cache 1 "new_value1";
  let result = get cache 1 in
  print_s [%sexp (result : string option)];
  [%expect {| (new_value1) |}]
;;
