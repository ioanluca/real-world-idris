import OCaml.IO

%lib malfunction "duration"
%lib malfunction "lwt"

Lwt : Type -> Type
Lwt = Abstr1

Int64 : Type
Int64 = Ptr

lwtReturn : {auto ta : OCaml_Types a} -> a -> OCaml_IO (Lwt a)
lwtReturn {a} x = ocamlCall "Lwt.return" (a -> OCaml_IO (Lwt a)) x

of_sec : Int -> Int64
of_sec n =
  unsafePerformIO $ ocamlCall "Duration.of_sec" (Int -> OCaml_IO Int64) n

lwtBind : {auto ta : OCaml_Types a} ->
          Lwt a ->
          (a -> OCaml_IO (Lwt b)) ->
          OCaml_IO (Lwt b)
lwtBind {a}{b} p f =
  ocamlCall "Lwt.bind" (Lwt a -> (a -> OCaml_IO (Lwt b)) -> OCaml_IO (Lwt b)) p f

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
       lwtThread `lwtBind` (\ () => loop $ n - 1)

     start : () -> OCaml_IO (Lwt ())
     start _ = loop 4


exports : FFI_Export FFI_OCaml "idrikernel.mli" []
exports = Fun Hello "Hello" End

main : OCaml_IO ()
main = pure ()
