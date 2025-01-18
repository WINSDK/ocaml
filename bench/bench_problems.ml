open! Core

module Time = Time_float_unix

(*
┌──────────────────────────────────────────────────────────────────┬───────────────┬──────────────────┐
│ Name                                                             │     Cycls/Run │         Time/Run │
├──────────────────────────────────────────────────────────────────┼───────────────┼──────────────────┤
│ [bench/bench_problems.ml] something that takes 200ms             │ 4_906_187.40c │ 204_425_778.40ns │
│ [bench/bench_problems.ml] reversing an array of 1234567 elements │   138_186.49c │   5_758_169.88ns │
│ [bench/bench_problems.ml] reversing a list:128                   │         8.13c │         339.04ns │
│ [bench/bench_problems.ml] reversing a list:1024                  │        63.84c │       2_662.89ns │
│ [bench/bench_problems.ml] reversing a list:16384                 │     1_198.03c │      49_942.51ns │
│ [bench/bench_problems.ml] reversing a list:262144                │    80_652.50c │   3_360_746.90ns │
└──────────────────────────────────────────────────────────────────┴───────────────┴──────────────────┘
*)

let %bench "something that takes 200ms" = Time.pause (Time.Span.of_ms 200.);;

let%bench_fun "reversing an array of 1234567 elements" =
  let rec v acc count =
    if count > 0 then
      v (Random.int Int.max_value :: acc) (count - 1)
    else
      acc
  in
  let v = List.to_array (v [] 1234567) in
  fun () ->
    let x = Array.rev v in
    x.(Array.length x - 1) (* Force `v` to be allocated. *)
;;

let%bench_fun "reversing a list" [@indexed len = [ 128; 1024; 16 * 1024; 256 * 1024; ]] =
  let rec v acc count =
    if count > 0 then
      v (Random.int Int.max_value :: acc) (count - 1)
    else
      acc
  in
  let v = v [] len in
  fun () ->
    let x = List.rev v in
    ignore (List.nth x (List.length x - 1)) (* Force `v` to be allocated. *)
;;
