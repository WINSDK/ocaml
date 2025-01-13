open! Base;;
open Stdio;;

let%test "rev" =
  let xs = [1; 3; 1] in
  let ys = List.rev xs in
  List.equal Int.equal xs ys
;;

(*
  When an expect_test ends up printing a mismatching output but the output is correct.
  You can run `dune promote` to change the %expect blocks to match the given output.
*)

let%expect_test "multi-block" =
  printf "Hello";
  [%expect {| Hello |}];
  printf "World!";
  [%expect {| World! |}]
;;

let%test "memset" =
  true;;
