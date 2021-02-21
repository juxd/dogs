open Base

type 'a result = 'a

module T = struct
  type ('a, 's) t = 's -> 'a result

  let return a _s = a
  let map = `Custom (fun t ~f s -> f (t s))
  let bind t ~f s = f (t s) s
end

include T
include (Monad.Make2 (T) : Monad.S2 with type ('a, 's) t := ('a, 's) t)

type 'a finalised = { f : 's. ('a, 's) t }

let run { f } = f ()

module Local_ref = struct
  type ('a, 's) t = 'a ref [@@deriving sexp_of]

  let create a = return (ref a)
  let read t = return !t
  let write t a = return (t := a)
end

module Local_array = struct
  type ('a, 's) t = 'a Array.t [@@deriving sexp_of]

  let of_list l = return (Array.of_list l)
  let size t = return (Array.length t)
  let get t i = return t.(i)
  let set t i a = return (t.(i) <- a)
  let swap t i j = return (Array.swap t i j)
  let to_list t = return (Array.to_list t)
end
