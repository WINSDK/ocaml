open! Core

module Time = Time_float_unix

(*
  let%bench =
    Some code that will be benchmarked.

  let%bench_fun =
    Some setup code.
    fun () ->
      Code that will be benched.

┌──────────────────────────────────────────────────────────────────┬───────────────┬──────────┬──────────┬───────────────┬────────────┐
│ Name                                                             │      Prom/Run │ mjWd/Run │ Time/Run │       mWd/Run │ Percentage │
├──────────────────────────────────────────────────────────────────┼───────────────┼──────────┼──────────┼───────────────┼────────────┤
│ [bench/bench_problems.ml] something that takes 0.5s              │               │          │ 503.42ms │         5.00w │    100.00% │
│ [bench/bench_problems.ml] reversing an array of 1234567 elements │         4.19w │   1.23Mw │   5.91ms │         3.00w │      1.17% │
│ [bench/bench_problems.ml] reversing a list:128                   │ 3_061_686.33w │   3.06Mw │  20.68ms │ 3_703_704.00w │      4.11% │
│ [bench/bench_problems.ml] reversing a list:1024                  │ 3_080_754.93w │   3.08Mw │  20.61ms │ 3_703_704.00w │      4.09% │
│ [bench/bench_problems.ml] reversing a list:16384                 │ 3_080_754.93w │   3.08Mw │  20.84ms │ 3_703_704.00w │      4.14% │
│ [bench/bench_problems.ml] reversing a list:262144                │ 3_085_519.12w │   3.09Mw │  21.25ms │ 3_703_704.00w │      4.22% │
└──────────────────────────────────────────────────────────────────┴───────────────┴──────────┴──────────┴───────────────┴────────────┘
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
