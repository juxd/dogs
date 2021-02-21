open Base

module Local_ref = struct
  module type S = sig
    type ('a, 's) st_monad
    type ('a, 's) t [@@deriving sexp_of]

    val create : 'a -> (('a, 's) t, 's) st_monad
    val read : ('a, 's) t -> ('a, 's) st_monad
    val write : ('a, 's) t -> 'a -> (unit, 's) st_monad
  end
end

module Local_array = struct
  module type S = sig
    type ('a, 's) st_monad
    type ('a, 's) t [@@deriving sexp_of]

    val of_list : 'a list -> (('a, 's) t, 's) st_monad
    val size : ('a, 's) t -> (int, 's) st_monad
    val get : ('a, 's) t -> int -> ('a, 's) st_monad
    val set : ('a, 's) t -> int -> 'a -> (unit, 's) st_monad
    val swap : ('a, 's) t -> int -> int -> (unit, 's) st_monad
    val to_list : ('a, 's) t -> ('a list, 's) st_monad
  end
end
