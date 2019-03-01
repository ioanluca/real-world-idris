open OS
open Lwt.Infix

module Echo_server (C: Mirage_console_lwt.S) (R: Mirage_random.C) = struct

  let read_line () =
    OS.Time.sleep_ns (Duration.of_ms (Randomconv.int ~bound:2500 R.generate))
    >|= fun () ->
    String.make (Randomconv.int ~bound:20 R.generate) 'a'

  let start c _r =
    let rec echo_server = function
      | 0 -> Lwt.return ()
      | n ->
        read_line () >>= fun s ->
        C.log c s >>= fun () ->
        echo_server (n - 1)
    in
    echo_server 10

end
