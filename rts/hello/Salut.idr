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
          (Step (MkOCamlFn (\t => modGet 0 time )) Stop))

-- start : 

-- exports : FFI_Export FFI_OCaml "salut.mli" []
-- exports = Data (OCaml_FnTypes Int) "ListInt" $ Fun f "f" End

main : OCaml_IO ()
main = printLn' "a"
    