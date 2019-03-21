import OCaml.IO

TimeSig : Type
TimeSig = OCamlModule [OCamlFn (Int -> OCaml_IO Ptr)] -- 0: sleep_ns

HelloFunctorSig : Type
HelloFunctorSig = TimeSig -> 
                  OCamlModule [OCamlFn (Ptr -> OCaml_IO Ptr)]

lwtUnit : OCaml_IO Ptr
lwtUnit = ocamlCall "Lwt.return_unit" (OCaml_IO Ptr)

of_sec : Int -> Int 
of_sec n = 
    unsafePerformIO $ ocamlCall "Duration.of_sec" (Int -> OCaml_IO Int) n

lwtBind : Ptr -> (Ptr -> OCaml_IO Ptr) -> OCaml_IO Ptr
lwtBind p f =
     ocamlCall "Lwt.bind" (Ptr -> OCamlFn (Ptr -> OCaml_IO Ptr) -> OCaml_IO Ptr) p (MkOCamlFn f)

start : TimeSig -> Ptr -> OCaml_IO Ptr
start timeMod t = loop 4
    where loop : Int -> OCaml_IO Ptr
          loop 0 = lwtUnit
          loop n = do
            putStrLn' "Idris Unikernel Hello!"
            (MkOCamlFn sleep) <- modGet 0 timeMod
            lwtThread <- sleep (of_sec 1) 
            lwtThread `lwtBind` (\ _ => loop $ n - 1)



HelloFunctor : HelloFunctorSig
HelloFunctor time = let defs = Step (MkOCamlFn $ start time) Stop 
       in unsafePerformIO $ mkMod defs


exports : FFI_Export FFI_OCaml "idrikernel.mli" []
exports = Fun HelloFunctor "Hello" End

main : OCaml_IO ()
main = pure ()