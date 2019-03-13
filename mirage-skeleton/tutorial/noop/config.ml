open Mirage

let main = foreign
  ~packages:[package "idrmirage"]
  "Unikernel" job

let () =
  register "noop" [main]
