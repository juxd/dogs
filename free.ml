open Base

module type Basic = sig
  type 'a t
end

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

module Make (F : Basic) = struct
  module T = struct
    type _ t =
      | Return : 'a -> 'a t
      | Suspended : 'a F.t -> 'a t
      | Binded : 'b t * ('b -> 'a t) -> 'a t

    let suspend f = Suspended f
    let return a = Return a
    let bind t ~f = Binded (t, f)

    let map =
      `Custom
        (fun t ~f ->
          match t with
          | Return a -> Return (f a)
          | (Suspended _ | Binded _) as t -> Binded (t, fun a -> Return (f a)))
    ;;
  end

  include T
  include (Monad.Make (T) : Monad.S with type 'a t := 'a t)

  module With_transformation_to_monad = struct
    module Arity1
        (M : Monad.S)
        (T : Transformation.S with type 'a s = 'a F.t and type 'a t = 'a M.t) =
    struct
      let rec fold_m = function
        | Return a -> M.return a
        | Suspended t -> T.transform t
        | Binded (Return a, f) -> fold_m (f a)
        | Binded (Suspended t, f) -> M.(T.transform t >>= fun a -> fold_m (f a))
        | Binded (Binded (t, f), f') -> fold_m (t >>= fun a -> f a >>= f')
      ;;
    end

    module Arity2
        (M : Monad.S2)
        (T : Transformation.S2 with type 'a s = 'a F.t and type ('a, 'r) t = ('a, 'r) M.t) =
    struct
      let rec fold_m = function
        | Return a -> M.return a
        | Suspended t -> T.transform t
        | Binded (Return a, f) -> fold_m (f a)
        | Binded (Suspended t, f) -> M.(T.transform t >>= fun a -> fold_m (f a))
        | Binded (Binded (t, f), f') -> fold_m (t >>= fun a -> f a >>= f')
      ;;
    end
  end
end
