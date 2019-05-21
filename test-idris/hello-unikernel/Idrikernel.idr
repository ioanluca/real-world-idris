import OCaml.IO

%lib malfunction "duration"
%lib malfunction "lwt"

Lwt : Type -> Type
Lwt _ = Ptr

Int64 : Type
Int64 = Ptr

lwtReturn : a -> OCaml_IO (Lwt a)
lwtReturn x = ocamlCall "Lwt.return" (Ptr -> OCaml_IO Ptr) (believe_me x)

of_sec : Int -> Int64
of_sec n =
  unsafePerformIO $ ocamlCall "Duration.of_sec" (Int -> OCaml_IO Int64) n

lwtBind : {auto ta : OCaml_Types a} ->
          Lwt a ->
          (a -> OCaml_IO Ptr) ->
          OCaml_IO Ptr
lwtBind {a} p f =
  ocamlCall "Lwt.bind" (Lwt a -> (a -> OCaml_IO Ptr) -> OCaml_IO Ptr) p f

print_endline : String -> OCaml_IO ()
print_endline s =
  ocamlCall "print_endline" (String -> OCaml_IO ()) s

HelloSig : Type
HelloSig =
  sig [Int64 -> OCaml_IO (Lwt ())] ->
  sig [() -> OCaml_IO (Lwt ())]

Hello : HelloSig
Hello time = struct [start]
  where
     sleep : Int64 -> OCaml_IO (Lwt ())
     sleep = modGet 0 time

     loop : Int -> OCaml_IO (Lwt ())
     loop 0 = lwtReturn ()
     loop n = do
       print_endline "Idris Unikernel Hello!"
       lwtThread <- sleep (of_sec 1)
       lwtThread `lwtBind` (\ _ => loop $ n - 1)

     start : () -> OCaml_IO (Lwt ())
     start _ = loop 4


exports : FFI_Export FFI_OCaml "idrikernel.mli" []
exports = Fun Hello "Hello" End

main : OCaml_IO ()
main = pure ()
