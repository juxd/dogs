open Base
type 'a result

include Monad.S2 with type ('a, 's) t = 's -> 'a result

type 'a finalised = { f : 's. ('a, 's) t }

module Local_ref : sig
    type ('a, 's) thread
    type ('a, 's) t [@@deriving sexp_of]

    val create : 'a -> (('a, 's) t, 's) thread
    val read : ('a, 's) t -> ('a, 's) thread
    val write : ('a, 's) t -> 'a -> (unit, 's) thread
  end with type ('a, 's) thread := ('a, 's) t

module Local_array : sig
    type ('a, 's) thread
    type ('a, 's) t [@@deriving sexp_of]

    val of_list : 'a list -> (('a, 's) t, 's) thread
    val size : ('a, 's) t -> (int, 's) thread
    val get : ('a, 's) t -> int -> ('a, 's) thread
    val set : ('a, 's) t -> int -> 'a -> (unit, 's) thread
    val swap : ('a, 's) t -> int -> int -> (unit, 's) thread
    val to_list : ('a, 's) t -> ('a list, 's) thread
  end with type ('a, 's) thread := ('a, 's) t

val run : 'a finalised -> 'a
