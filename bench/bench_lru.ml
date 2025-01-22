open Core
open Core_bench
open Problems

let setup_cache ~size =
  let cache = Lru.create ~size (module Int) (module Int) in
  for i = 1 to size do
    Lru.put cache i (i * i)
  done;
  cache
;;

let cache_size = 10_000
let cache = setup_cache ~size:cache_size

(* Benchmark for the "put" operation *)
let%bench "put operation" [@indexed size = [ 100; 1_000; 10_000 ]] =
  for i = 1 to size do
    Lru.put cache i (i * i)
  done
;;

(* Benchmark for the "get (cache hit)" operation *)
let%bench "get (cache hit)" [@indexed size = [ 100; 1_000; 10_000 ]] =
  for i = 1 to size do
    ignore (Lru.get cache i : int option)
  done
;;

(* Benchmark for the "get (cache miss)" operation *)
let%bench "get (cache miss)" [@indexed size = [ 100; 1_000; 10_000 ]] =
  for i = cache_size + 1 to cache_size + size do
    ignore (Lru.get cache i : int option)
  done
;;

(* Benchmark for the "eviction on put" operation *)
let%bench "eviction on put" [@indexed size = [ 100; 1_000; 10_000 ]] =
  let small_cache = Lru.create ~size:100 (module Int) (module Int) in
  for i = 1 to size do
    Lru.put small_cache i (i * i)
  done
;;
