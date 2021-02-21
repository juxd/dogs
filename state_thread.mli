open Base
include Monad.S2 with type ('a, 's) t = 's -> 'a

type 'a final_state = { f : 's. ('a, 's) t }

val run : 'a final_state -> 'a
