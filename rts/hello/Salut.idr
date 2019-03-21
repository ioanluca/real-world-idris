import OCaml.IO

ModuleTy : Type
ModuleTy = OCamlModule [ OCamlFn (Int -> Int) ] ->
           OCamlModule [ OCamlFn (Int -> String)
                       , String
                       , OCamlFn (Ptr -> Ptr) ]


Unmodul : ModuleTy
Unmodul time = 
   let vs = Step  (MkOCamlFn (\k => "ok")) 
          (Step " pa"
          (Step (MkOCamlFn (\t => unsafePerformIO $ do
            ocamlCall
               "Pervasives.print_endline" (String -> OCaml_IO ()) "saluuutt!!!!!!" 
            ocamlCall "Lwt.return_unit"  (OCaml_IO Ptr)))
          Stop))
   in unsafePerformIO $ mkMod vs

DummyTimeModule : OCamlModule [ OCamlFn (Int -> Int)]
DummyTimeModule = unsafePerformIO $ 
    mkMod $ Step (MkOCamlFn (+1)) Stop 

f : (Maybe Bool) -> OCamlModule [ OCamlFn (Int -> String)
               , String
               , OCamlFn (Ptr -> Ptr) ]
    -> String
f Nothing _ = "showing module nth"
f (Just True) _ = "showing module true"
f (Just False) _ = "showing module false"


exports : FFI_Export FFI_OCaml "salut.mli" []
exports = Fun Unmodul "Unmodul" $
          Fun f "f" $ End

main : OCaml_IO ()
main = do
    let asd = Unmodul DummyTimeModule
    putStrLn' (f (Just True) asd)
    putStrLn' (f (Just False) asd)
    putStrLn' (f Nothing asd)

-- Obj operations are primitives and are not implemented in malfunction
-- step values get erased, I think
    