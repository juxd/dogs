open Base

module Make (F : T1) = struct
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

  type 'a f = 'a F.t

  include T
  include (Monad.Make (T) : Monad.S with type 'a t := 'a t)

  module To_monad = struct
    module Arity1
        (M : Monad.S)
        (T : Free_intf.Transformation.S with type 'a s = 'a f and type 'a t = 'a M.t) =
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
        (T : Free_intf.Transformation.S2
               with type 'a s = 'a f
                and type ('a, 'r) t = ('a, 'r) M.t) =
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
