import OCaml.IO
import OCaml.Lwt

%lib malfunction "cohttp"
%lib malfunction "cohttp-lwt"
%lib malfunction "cohttp-lwt-unix"
%lib malfunction "uri"
%lib malfunction "lwt.unix"

-----------------------------------------------------------------------------

Uri : Type
Uri = Ptr

uri_to_string : Uri -> OCaml_IO String
uri_to_string u =
  ocamlCall "Uri.to_string" (Uri -> OCaml_IO String) u

-----------------------------------------------------------------------------

Request : Type
Request = Ptr

uri : Request -> OCaml_IO Uri
uri r = ocamlCall "Cohttp.Request.uri" (Request -> OCaml_IO Uri) r

Response : Type
Response = Ptr

Body : Type
Body = Ptr

body_to_string_lwt : Body -> OCaml_IO (Lwt String)
body_to_string_lwt b =
  ocamlCall "Cohttp_lwt.Body.to_string" (Body -> OCaml_IO (Lwt String)) b

Meth : Type
Meth = Ptr

meth : Request -> OCaml_IO Meth
meth r = ocamlCall "Cohttp.Request.meth" (Request -> OCaml_IO Meth) r

string_of_method : Meth -> OCaml_IO String
string_of_method m =
  ocamlCall "Cohttp.Code.string_of_method" (Meth -> OCaml_IO String) m

Header : Type
Header = Ptr

headers : Request -> OCaml_IO Header
headers r =
   ocamlCall "Cohttp.Request.headers" (Request -> OCaml_IO Header) r

header_to_string : Header -> OCaml_IO String
header_to_string h =
  ocamlCall "Cohttp.Header.to_string" (Meth -> OCaml_IO String) h

-----------------------------------------------------------------------------

OK : Int
OK = 17692

server_respond_string : Maybe Bool -> Maybe Header -> Int ->
                        String -> () ->
                        OCaml_IO (Lwt (Response, Body))
server_respond_string flush headers status body () =
  ocamlCall "Cohttp_lwt_unix.Server.respond_string"
      (Maybe Bool -> Maybe Header -> Int -> String -> () -> OCaml_IO (Lwt (Response, Body)))
      flush headers status body ()

Server : Type
Server = Ptr

server_create : Maybe Int -> -- timeout
                Maybe Int -> -- backlog
                Maybe (Lwt ()) -> -- stop
                Maybe (Ptr -> OCaml_IO ()) -> -- on_exn
                Maybe Ptr -> -- ctx
                Maybe Ptr -> -- mode
                Server ->
                OCaml_IO (Lwt ())
server_create timeout backlog stop on_exn ctx mode server =
  ocamlCall "Cohttp_lwt_unix.Server.create"
    (Maybe Int -> -- timeout
     Maybe Int -> -- backlog
     Maybe (Lwt ()) -> -- stop
     Maybe (Ptr -> OCaml_IO ()) -> -- on_exn
     Maybe Ptr -> -- ctx
     Maybe Ptr -> -- mode
     Server ->
     OCaml_IO (Lwt ()))
    timeout backlog stop on_exn ctx mode server

server_make : Maybe (Ptr -> OCaml_IO ()) -> -- conn_closed
              (Ptr -> Request -> Body -> OCaml_IO (Lwt (Response, Body))) ->
              () ->
              OCaml_IO Server
server_make conn_closed callback () =
  ocamlCall "Cohttp_lwt_unix.Server.make"
     (Maybe (Ptr -> OCaml_IO ()) -> -- conn_closed
      (Ptr -> Request -> Body -> OCaml_IO (Lwt (Response, Body))) ->
      () ->
      OCaml_IO Server)
     conn_closed callback ()

lwtRun : Lwt () -> OCaml_IO ()
lwtRun lwt = ocamlCall "Lwt_main.run" (Lwt () -> OCaml_IO ()) lwt

-----------------------------------------------------------------------------

callback : Ptr -> Request -> Body -> OCaml_IO (Lwt (Response, Body))
callback conn req body = do
  uri <- pure req >>= uri >>= uri_to_string
  meth <- pure req >>= meth >>= string_of_method
  headers <- pure req >>= headers >>= header_to_string
  print_endline ("URI: " ++ uri)
  print_endline ("Method: " ++ meth)
  print_endline ("Headers: " ++ headers)
  lwtb <- body_to_string_lwt body
  lwtb `lwtBind`
    (\ body_str =>
        let response = unlines [ "Uri: " ++ uri
                               , "Method: " ++ meth
                               , "Headers: " ++ headers
                               , "Body: " ++ body_str
                               ]
        in
        server_respond_string Nothing Nothing OK response ())

main : OCaml_IO ()
main = do
  server <- server_make Nothing callback ()
  thread <- server_create Nothing Nothing Nothing Nothing Nothing Nothing server
  lwtRun thread
