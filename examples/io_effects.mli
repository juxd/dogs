(* See comments in the [.ml] file to understand what these interfaces mean. *)
open Dogs

module T : sig
  type _ t =
    | Read_line : string option t
    | Print_endline : string -> unit t
end

module String_buffers : sig
  type t =
    { input : string list
    ; output : string list
    }
  [@@deriving sexp_of, fields]

  val get_next_input : t -> t * string option
  val add_output : t -> s:string -> t

  module State : Travesty.State_types.S with type state := t
end

module Free : sig
  include Free_intf.S with type 'a f = 'a T.t

  module To_cont : sig
    val fold_m : 'a t -> ('a, _) Cont.t
  end

  module To_string_buffers_state : sig
    val fold_m : 'a t -> 'a String_buffers.State.t
  end
end
