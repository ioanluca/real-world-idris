import OCaml.IO

ModuleTy : Type
ModuleTy = OCamlModule [ OCamlFn (Int -> Ptr) ] ->
           OCamlModule [ OCamlFn (Int -> String)
                       , String
                       , OCamlFn (Ptr -> Ptr) ]


Unmodul : ModuleTy
Unmodul time = 
  unsafePerformIO $ mkMod $ Step (MkOCamlFn (\k => "sdkjhsdf")) 
          (Step "ok"
          (Step (MkOCamlFn (\t => unsafePerformIO $ modGet 0 time )) Stop))


exports : FFI_Export FFI_OCaml "salut.mli" []
exports = Fun Unmodul "Unmodul" End

main : OCaml_IO ()
main = printLn' "a"
    