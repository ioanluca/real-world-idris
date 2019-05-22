module OCaml.Lwt

import OCaml.IO

%default total
%access public export

%lib malfunction "lwt"

Lwt : Type -> Type
Lwt = Abstr1

lwtReturn : {auto ta : OCaml_Types a} -> a -> OCaml_IO (Lwt a)
lwtReturn {a} x = ocamlCall "Lwt.return" (a -> OCaml_IO (Lwt a)) x

lwtBind : {auto ta : OCaml_Types a} ->
          Lwt a ->
          (a -> OCaml_IO (Lwt b)) ->
          OCaml_IO (Lwt b)
lwtBind {a}{b} p f =
  ocamlCall "Lwt.bind" (Lwt a -> (a -> OCaml_IO (Lwt b)) -> OCaml_IO (Lwt b)) p f
