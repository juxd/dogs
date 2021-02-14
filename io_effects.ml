open Base

module Actions = struct
  type _ t =
    | Read_line : string option t
    | Print_endline : string -> unit t
end

module Io_free = struct
  include Free.Make (Actions)

  module Cont_transformation = struct
    type 'a s = 'a Actions.t
    type ('a, 'r) t = ('a, 'r) Cont.t

    let transform : type a. a s -> (a, 'r) t = function
      | Actions.Read_line ->
        Cont.from_thunk (fun () -> Stdio.(In_channel.input_line stdin))
      | Print_endline s -> Cont.from_thunk (fun () -> Stdio.print_endline s)
    ;;
  end

  module To_cont = To_monad.Arity2 (Cont) (Cont_transformation)

  let%test_module "test cont" =
    (module struct
      let left_nested_bind ~n =
        let rec left_nested_bind' t ~n =
          if n <= 0
          then t
          else
            left_nested_bind'
              (let open Let_syntax in
              let%bind () = t in
              suspend (Actions.Print_endline (Printf.sprintf "print #%d" n)))
              ~n:(n - 1)
        in
        left_nested_bind' (return ()) ~n
      ;;

      let right_nested_bind ~n =
        let rec right_nested_bind' t ~n =
          if n <= 0
          then t
          else
            right_nested_bind'
              (let open Let_syntax in
              let%bind () =
                suspend (Actions.Print_endline (Printf.sprintf "print #%d" n))
              in
              t)
              ~n:(n - 1)
        in
        right_nested_bind' (return ()) ~n
      ;;

      let%expect_test _ =
        To_cont.fold_m (left_nested_bind ~n:5) |> Cont.get;
        [%expect
          {|
          print #5
          print #4
          print #3
          print #2
          print #1 |}];
        To_cont.fold_m (right_nested_bind ~n:5) |> Cont.get;
        [%expect
          {|
          print #1
          print #2
          print #3
          print #4
          print #5 |}]
      ;;
    end)
  ;;

  module Stub_buffers_transformation = struct
    type 'a s = 'a Actions.t
  end
end
