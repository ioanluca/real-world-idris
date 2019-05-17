import OCaml.IO

%lib malfunction "lwt"
%lib malfunction "cohttp"
%lib malfunction "cohttp-lwt"
%lib malfunction "uri"
%lib malfunction "cohttp-lwt-unix"


Lwt : Type -> Type
Lwt _ = Ptr

Request : Type
Request = Ptr 

Response : Type
Response = Ptr

Body : Type
Body = Ptr 


-- lwtUnit : OCaml_IO Ptr
-- lwtUnit = ocamlCall "Lwt.return_unit" (OCaml_IO Ptr)

lwtReturn : Ptr -> OCaml_IO (Lwt Ptr)
lwtReturn = ocamlCall "Lwt.return" (Ptr -> OCaml_IO Ptr)

-- lwtBind : Lwt a -> (a -> OCaml_IO (Lwt b)) -> OCaml_IO (Lwt b)
lwtBind : Ptr -> (Ptr -> OCaml_IO Ptr) -> OCaml_IO Ptr
-- lwtBind : String -> (String -> OCaml_IO (Response, Body)) 
--           -> OCaml_IO (Response, Body)
lwtBind p f =  ocamlCall "Lwt.bind"
  -- (String -> OCamlFn (String -> OCaml_IO (Response, Body)) -> OCaml_IO (Response, Body))
  (Ptr -> OCamlFn (Ptr -> OCaml_IO Ptr) -> OCaml_IO Ptr)
  p (MkOCamlFn f)



Uri : Type 
Uri = Ptr

uri : Request -> OCaml_IO Uri
uri r = ocamlCall "Cohttp.Request.uri" (Request -> OCaml_IO Uri) r

uri_to_string : Uri -> OCaml_IO String 
uri_to_string u = 
  ocamlCall "Uri.to_string" (Uri -> OCaml_IO String) u

-- body_to_string_lwt : Body -> OCaml_IO (Lwt String)
body_to_string_lwt : Body -> OCaml_IO (Lwt Ptr)
body_to_string_lwt b = 
  ocamlCall "Cohttp_lwt.Body.to_string"
  (Body -> OCaml_IO (Lwt Ptr)) b
  -- (Body -> OCaml_IO (Lwt String)) b

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

OK : Int
OK = 17692

server_respond_string : Int -> Int -> Int -> 
                        Ptr -> () ->  
--                         String -> () -> 
                        OCaml_IO Ptr
                        -- OCaml_IO (Response, Body)
server_respond_string flush headers status body () =  
  ocamlCall "Cohttp_lwt_unix.Server.respond_string"
        ( Int -> Int -> Int -> 
          Ptr -> () ->  
        --   String -> () -> 
          OCaml_IO Ptr )
          -- OCaml_IO (Response, Body) 
  flush headers status body ()

callback : Ptr -> Request -> Body -> OCaml_IO (Lwt (Response, Body))
-- callback : a -> Request -> Body -> OCaml_IO (Lwt Ptr)
callback conn req body = do 
  {-uri <- pure req >>= uri >>= uri_to_string
  meth <- pure req >>= meth >>= string_of_method
headers <- pure req >>= headers >>= header_to_string-}
  lwtb <- pure body >>= body_to_string_lwt
  lwtb `lwtBind` 
    (\ b => server_respond_string 0 0 OK b ())

exports : FFI_Export FFI_OCaml "idriscohttp.mli" []
exports = Fun callback "callback" End

main : OCaml_IO ()
main = pure ()

