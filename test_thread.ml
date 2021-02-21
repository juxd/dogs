open Base
open State_thread

let%expect_test "check that normal t's can compile" =
  run { f = (fun s -> return 3 s) } |> Stdio.printf "%d\n";
  [%expect {| 3 |}];
  let chained_monad x y =
    let open Let_syntax in
    let%bind x = return x in
    let%map y = return y in
    Printf.sprintf "%d + %d = %d" x y (x + y)
  in
  run { f = (fun s -> chained_monad 1 2 s) } |> Stdio.print_endline;
  [%expect {| 1 + 2 = 3 |}]
;;

let%expect_test "check that t's made with local ref or array work properly" =
  (* We need to eta-expand these operations to prevent weak polymorphism. *)
  let local_ref s = Local_ref.create 3 s in
  let local_array s = Local_array.of_list [ 1; 2; 3 ] s in
  (* We expect uncommenting the following causes compilation to {b fail }. *)
  (* let local_ref_res = run { f = (fun s -> local_ref s) } in
   * Stdio.print_s ([%sexp_of: (int, _) Local_ref.t] local_ref_res);
   * [%expect {| 3 |}];
   * let local_array_res = run { f = (fun s -> local_array s) } in
   * Stdio.print_s ([%sexp_of: (int, _) Local_array.t] local_array_res);
   * [%expect {| (1 2 3) |}]; *)
  let increment_contents int_ref =
    let open Let_syntax in
    let%bind x = Local_ref.read int_ref in
    let%bind () = Local_ref.write int_ref (x + 1) in
    let%map new_x = Local_ref.read int_ref in
    new_x
  in
  run { f = (fun s -> (local_ref >>= increment_contents) s) } |> Stdio.printf "%d\n";
  [%expect {| 4 |}];
  let swap_contents_and_convert_to_list arr =
    let open Let_syntax in
    let%bind () = Local_array.swap arr 0 1 in
    let%map ls = Local_array.to_list arr in
    ls
  in
  run { f = (fun s -> (local_array >>= swap_contents_and_convert_to_list) s) }
  |> fun ls ->
  Stdio.print_s ([%sexp_of: int list] ls);
  [%expect {| (2 1 3) |}]
;;
