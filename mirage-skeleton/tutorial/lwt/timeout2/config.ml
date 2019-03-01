open Mirage

let packages = [package "duration"; package "randomconv"]

let () =
  let main = foreign ~packages "Unikernel.Timeout2" (console @-> random @-> job) in
  register "timeout2" [ main $ default_console $ default_random ]
