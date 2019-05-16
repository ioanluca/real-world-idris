open Mirage

let main =
  foreign
    ~packages:[package "duration"; package "idrikernel"]
    "Idrikernel.Hello" (time @-> job)

let () =
  register "idrikernel" [main $ default_time]