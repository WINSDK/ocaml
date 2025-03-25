open! Core
open! Problems.Lru

module M = Make (Int)

let setup_cache ~size =
  let cache = M.create ~size () in
  for i = 1 to size do
    M.add cache ~key:i ~data:(i * i)
  done;
  cache
;;

let cache_size = 10_000
let cache = setup_cache ~size:cache_size

(* Benchmark for the "put" operation *)
let%bench "put operation" [@indexed size = [ 100; 1_000; 10_000 ]] =
  for i = 1 to size do
    M.add cache ~key:i ~data:(i * i)
  done
;;

(* Benchmark for the "get (cache hit)" operation *)
let%bench "get (cache hit)" [@indexed size = [ 100; 1_000; 10_000 ]] =
  for i = 1 to size do
    ignore (M.get cache i : int option)
  done
;;

(* Benchmark for the "get (cache miss)" operation *)
let%bench "get (cache miss)" [@indexed size = [ 100; 1_000; 10_000 ]] =
  for i = cache_size + 1 to cache_size + size do
    ignore (M.get cache i : int option)
  done
;;

(* Benchmark for the "eviction on put" operation *)
let%bench "eviction on put" [@indexed size = [ 100; 1_000; 10_000 ]] =
  let small_cache = M.create ~size:100 () in
  for i = 1 to size do
    M.add small_cache ~key:i ~data:(i * i)
  done
;;
