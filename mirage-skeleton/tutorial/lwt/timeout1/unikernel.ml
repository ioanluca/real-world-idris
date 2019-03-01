open OS
open Lwt.Infix

module Timeout1 (C: Mirage_console_lwt.S) (R: Mirage_random.C) = struct

  let timeout delay t =
    Time.sleep_ns delay >>= fun () ->
    match Lwt.state t with
    | Lwt.Sleep    -> Lwt.cancel t; Lwt.return None
    | Lwt.Return v -> Lwt.return (Some v)
    | Lwt.Fail ex  -> Lwt.fail ex

  let start c _r =
    let t =
      Time.sleep_ns (Duration.of_ms (Randomconv.int ~bound:3000 R.generate))
      >|= fun () -> "Heads"
    in
    timeout (Duration.of_sec 2) t >>= function
    | None   -> C.log c "Cancelled"
    | Some v -> C.log c (Printf.sprintf "Returned %S" v)

end
