open Base

module T = struct
  type ('a, 'r) t = ('a -> 'r) -> 'r

  let return a k = k a
  let map = `Define_using_bind
  let bind t ~f k = t (fun a -> f a k)
end

include T
include (Monad.Make2 (T) : Monad.S2 with type ('a, 'r) t := ('a, 'r) t)

let from_thunk a k = k @@ a ()
let get (t : ('a, _) t) = t Fn.id
let rec forever t = bind t ~f:(fun _ -> forever t)
let rec repeat t ~n = if n <= 1 then t else bind t ~f:(fun _ -> repeat t ~n:(n - 1))
