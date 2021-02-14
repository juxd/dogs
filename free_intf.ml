open Base

(** Represents transformations from one container type to another.  *)
module Transformation = struct
  module type S = sig
    type 'a s
    type 'a t

    val transform : 'a s -> 'a t
  end

  module type S2 = sig
    type 'a s
    type ('a, 'r) t

    val transform : 'a s -> ('a, _) t
  end
end

module type S = sig
  type 'a f
  type 'a t

  val suspend : 'a f -> 'a t

  include Monad.S with type 'a t := 'a t

  module To_monad : sig
    module Arity1 : functor
      (M : Monad.S)
      (T : Transformation.S with type 'a s := 'a f and type 'a t := 'a M.t)
      -> sig
      val fold_m : 'a t -> 'a M.t
    end

    module Arity2 : functor
      (M : Monad.S2)
      (T : Transformation.S2 with type 'a s := 'a f and type ('a, 'r) t := ('a, 'r) M.t)
      -> sig
      val fold_m : 'a t -> ('a, _) M.t
    end
  end
end
