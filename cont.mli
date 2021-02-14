open Base
include Monad.S2

val from_thunk : (unit -> 'a) -> ('a, _) t
val get : ('a, 'a) t -> 'a
val forever : ('a, 'r) t -> ('a, 'r) t
val repeat : ('a, 'r) t -> n:int -> ('a, 'r) t
