open Base
open Dogs

(** An example of how the Free monad can be used to implement a DSL for IO.  *)

(** {1 Defining the DSL}

    We first define a DSL to represent actions we would take. This action should
    be of type ['a t], which means that they should translatable to a monad
    implementation of type ['a Monad_implementation.t] *)
module T = struct
  type _ t =
    | Read_line : string option t
    | Print_endline : string -> unit t
end

(** [String_buffers.t] is but one of a possible monad we can transform DSL (of
    [T.t]) into. We will see how this translation is created in module [Free]. *)
module String_buffers = struct
  module T = struct
    type t =
      { input : string list
      ; output : string list
      }
    [@@deriving sexp_of, fields]

    let get_next_input t = { t with input = List.drop t.input 1 }, List.hd t.input
    let add_output t ~s = { t with output = s :: t.output }
  end

  include T
  module State : Travesty.State_types.S with type state := t = Travesty.State.Make (T)
end

module Free = struct
  (** {2 Making a Free monad}

      After defining the DSL, we make its Free monad using [Free.Make]. This
      will provide us with functors that convert the created monad into a given
      monad implementation. *)
  include Free.Make (T)

  (** {3 Defining a transformation}

      This is module implements the transformation from the DSL type to a Cont
      monad. *)
  module Cont_transformation = struct
    type 'a s = 'a f
    type ('a, 'r) t = ('a, 'r) Cont.t

    let transform : type a. a s -> (a, 'r) t = function
      | Read_line -> Cont.from_thunk (fun () -> Stdio.(In_channel.input_line stdin))
      | Print_endline s -> Cont.from_thunk (fun () -> Stdio.print_endline s)
    ;;
  end

  (** {4 Creating a converter to a cont}

      With the previously implemented transformation, we can use the
      [To_monad.Arity] functors to create a converter from a Free monad of our IO
      DSL into a [Cont.t] *)
  module To_cont = To_monad.Arity2 (Cont) (Cont_transformation)

  module String_buffers_state_transformation = struct
    type 'a s = 'a f
    type 'a t = 'a String_buffers.State.t

    let transform : type a. a s -> a t = function
      | Read_line -> String_buffers.State.make String_buffers.get_next_input
      | Print_endline s ->
        String_buffers.State.modify (fun t -> String_buffers.add_output t ~s)
    ;;
  end

  (** {5 Another example with our custom monad}

      This is an example of how we can use the free monad to implement testable
      input and outputs. *)
  module To_string_buffers_state =
    To_monad.Arity1 (String_buffers.State) (String_buffers_state_transformation)

  let%test_module "test transforms" =
    (module struct
      let left_nested_bind ~n ~bind_f =
        let rec left_nested_bind' t ~n =
          if n <= 0
          then t
          else
            left_nested_bind'
              (let open Let_syntax in
              t >>= bind_f n)
              ~n:(n - 1)
        in
        left_nested_bind' (return ()) ~n
      ;;

      let right_nested_bind ~n ~t_f =
        let rec right_nested_bind' t ~n =
          if n <= 0
          then t
          else
            right_nested_bind'
              (let open Let_syntax in
              let%bind () = t_f n in
              t)
              ~n:(n - 1)
        in
        right_nested_bind' (return ()) ~n
      ;;

      let%expect_test _ =
        To_cont.fold_m
          (left_nested_bind ~n:5 ~bind_f:(fun n () ->
               suspend (Print_endline (Printf.sprintf "print #%d" n))))
        |> Cont.get;
        [%expect
          {|
          print #5
          print #4
          print #3
          print #2
          print #1 |}];
        To_cont.fold_m
          (right_nested_bind ~n:5 ~t_f:(fun n ->
               suspend (Print_endline (Printf.sprintf "print #%d" n))))
        |> Cont.get;
        [%expect
          {|
          print #1
          print #2
          print #3
          print #4
          print #5 |}]
      ;;

      let buffer_with_input_sequence =
        String_buffers.Fields.create ~input:[ "print 1"; "print 2"; "print 3" ] ~output:[]
      ;;

      let print_or_report_empty = function
        | Some s -> T.Print_endline s
        | None -> Print_endline "[empty option]"
      ;;

      let%expect_test _ =
        let string_buffers, () =
          let string_buffer_state =
            To_string_buffers_state.fold_m
              (left_nested_bind ~n:4 ~bind_f:(fun _ () ->
                   let open Let_syntax in
                   let%bind s = suspend Read_line in
                   suspend (print_or_report_empty s)))
          in
          String_buffers.State.run' string_buffer_state buffer_with_input_sequence
        in
        Stdio.print_s ([%sexp_of: String_buffers.t] string_buffers);
        [%expect
          {|
          ((input ()) (output ("[empty option]" "print 3" "print 2" "print 1"))) |}];
        let string_buffers, () =
          let string_buffer_state =
            To_string_buffers_state.fold_m
              (right_nested_bind ~n:4 ~t_f:(fun _ ->
                   let open Let_syntax in
                   let%bind s = suspend Read_line in
                   suspend (print_or_report_empty s)))
          in
          String_buffers.State.run' string_buffer_state buffer_with_input_sequence
        in
        Stdio.print_s ([%sexp_of: String_buffers.t] string_buffers);
        [%expect
          {|
          ((input ()) (output ("[empty option]" "print 3" "print 2" "print 1"))) |}]
      ;;
    end)
  ;;
end
