import OCaml.IO
import OCaml.Lwt

%lib malfunction "duration"

Int64 : Type
Int64 = Ptr

of_sec : Int -> Int64
of_sec n =
  unsafePerformIO $ ocamlCall "Duration.of_sec" (Int -> OCaml_IO Int64) n

Hello : Module [val "sleep" (Int64 -> OCaml_IO (Lwt ()))] ->
        Module [val "start" (() -> OCaml_IO (Lwt ()))]
Hello time =
  struct [ Let "start" $ \() => loop 4 ]
  where
     loop : Int -> OCaml_IO (Lwt ())
     loop 0 = lwtReturn ()
     loop n = do
       print_endline "Idris Unikernel Hello!"
       lwtThread <- (time#"sleep") (of_sec 1)
       lwtThread `lwtBind` (\ () => loop $ n - 1)

exports : FFI_Export FFI_OCaml "idrikernel.mli" []
exports = Fun Hello "Hello" End

main : OCaml_IO ()
main = pure ()
