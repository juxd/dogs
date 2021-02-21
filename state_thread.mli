open Base
type 'a result

include Monad.S2 with type ('a, 's) t = 's -> 'a result

type 'a finalised = { f : 's. ('a, 's) t }

module Local_ref : State_thread_intf.Local_ref.S with type ('a, 's) st_monad := ('a, 's) t

module Local_array : State_thread_intf.Local_array.S with type ('a, 's) st_monad := ('a, 's) t

val run : 'a finalised -> 'a
