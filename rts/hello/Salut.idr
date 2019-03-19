import OCaml.IO

ModuleTy : Type
ModuleTy = OCamlModule [ OCamlFn (Int -> Ptr) ] ->
           OCamlModule [ OCamlFn (Int -> String)
                       , String
                       , OCamlFn (Ptr -> Ptr) ]

modGet : (i : Nat) -> OCamlModule tys -> a
-- modGet : (i : Nat) -> {auto index' i tys = Some a} -> OCamlModule tys -> a

data Values : List Type -> Type where
  Stop : Values []
  Step : t -> Values tys -> Values (t :: tys)

-- mkMod : Values tys -> OCaml_IO (OCamlModule tys)
-- mkMod vs = go vs 0 where
--   go : Values tys -> Int -> OCaml_IO (OCamlModule tys)
--   go {tys = []} Stop n = ocamlCall "Obj.new_block" (Int -> Int -> OCaml_IO (OCamlModule [])) 0 n 
--   go {tys = ty :: tys} (Step t ts) n = do 
--      m <- go ts (n + 1)
--      ocamlCall "Obj.set_field" (OCamlModule tys -> Int -> ty -> OCaml_IO (OCamlModule (ty :: tys))) m n 

mkMod : Values tys -> OCaml_IO (OCamlModule tys)
mkMod {tys = tys} vs = go vs 0 where
  go : Values tys2 -> Int -> OCaml_IO (OCamlModule tys)
  go {tys2 = []} Stop n = ocamlCall "Obj.new_block" (Int -> Int -> OCaml_IO (OCamlModule tys)) 0 n
  go {tys2 = ty :: tys2} (Step v vs) n = do
     m <- go vs (n + 1)
     ocamlCall "Obj.set_field" (OCamlModule tys -> Int -> OCamlRaw ty -> OCaml_IO ()) m n (MkOcamlRaw v)
     pure m

-- mkMod : Values tys -> OCaml_IO (OCamlModule tys)
-- mkMod {tys = tys} vs = ?asd

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
    