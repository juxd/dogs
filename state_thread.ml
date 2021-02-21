open Base

module T = struct
  type ('a, 's) t = 's -> 'a

  let return a _s = a
  let map = `Custom (fun t ~f s -> f (t s))
  let bind t ~f s = f (t s) s
end

type ('a, 's) t = ('a, 's) T.t

include (Monad.Make2 (T) : Monad.S2 with type ('a, 's) t := ('a, 's) t)

type 'a final_state = { f : 's. ('a, 's) t }

let run { f } = f ()

let%expect_test "check that normal thunks can compile" =
  run { f = (fun s -> return 3 s) } |> Stdio.printf "%d\n";
  [%expect{| 3 |}];
  let chained_monad x y =
    let open Let_syntax in
    let%bind x = return x in
    let%map y = return y in
    Printf.sprintf "%d + %d = %d" x y (x + y)
  in
  run { f = (fun s -> chained_monad 1 2 s) } |> Stdio.print_endline;
  [%expect{| 1 + 2 = 3 |}]
;;
