open Lwt
(* open Cohttp *)
(* open Cohttp_lwt_unix *)

let callback conn req body =
  Idriscohttp.callback conn req body ()

let server =
  (* let callback _conn req body =
    let uri = req |> Cohttp.Request.uri |> Uri.to_string in
    let meth = req |> Cohttp.Request.meth |> Cohttp.Code.string_of_method in
    let headers = req |> Cohttp.Request.headers |> Cohttp.Header.to_string in
    body |> Cohttp_lwt.Body.to_string >|= (fun body ->
      (Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s"
         uri meth headers body))
    >>= (fun body -> Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ())
  in *)
  Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port 8000)) (Cohttp_lwt_unix.Server.make ~callback:callback ())

let () = ignore (Lwt_main.run server)