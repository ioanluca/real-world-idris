module M = Idrikernel.Hello (Time)

let () =
  (* Printf.printf "M.test = %d\n" (M.test 101) *)
  Lwt_main.run (M.start ())
