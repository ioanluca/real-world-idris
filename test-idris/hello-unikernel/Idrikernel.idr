import OCaml.IO

Lwt : Type -> Type
Lwt _ = Ptr

Int64 : Type
Int64 = Ptr

TimeSig : Type
TimeSig = OCamlModule [ OCamlFn (Int64 -> {-OCaml_IO-} Ptr) ] -- 0: sleep_ns

HelloFunctorSig : Type
HelloFunctorSig = TimeSig ->
                  OCamlModule [OCamlFn (Int -> Int), OCamlFn ({-Ptr ->-} OCaml_IO Ptr)]

lwtUnit : OCaml_IO Ptr
lwtUnit = ocamlCall "Lwt.return_unit" (OCaml_IO Ptr)

lwtReturn : () -> OCaml_IO Ptr
lwtReturn = ocamlCall "Lwt.return" (() -> OCaml_IO Ptr)

of_sec : Int -> Int64
of_sec n =
    unsafePerformIO $ ocamlCall "Duration.of_sec" (Int -> OCaml_IO Int64) n

-- lwtBind : Lwt a -> (a -> OCaml_IO (Lwt b)) -> OCaml_IO (Lwt b)
lwtBind : Ptr -> (Ptr -> {-OCaml_IO-} Ptr) -> OCaml_IO Ptr
lwtBind p f =
  ocamlCall "Lwt.bind" (Ptr -> OCamlFn (Ptr -> {-OCaml_IO-} Ptr) -> OCaml_IO Ptr) p (MkOCamlFn f)

start : TimeSig -> OCaml_IO Ptr
start timeMod {-t-} = loop 4
    where loop : Int -> OCaml_IO Ptr
          loop 0 = lwtUnit
          loop n = do
            putStrLn' "Idris Unikernel Hello!"
            --lwtReturn ()
            (MkOCamlFn sleep) <- modGet 0 timeMod
            let lwtThread = sleep (of_sec 1)
            lwtThread `lwtBind` (\ _ => unsafePerformIO (loop $ n - 1))



HelloFunctor : HelloFunctorSig
HelloFunctor time = let defs = Step (MkOCamlFn (\x => x + 101)) (Step (MkOCamlFn $ start time) Stop)
       in unsafePerformIO $ mkMod defs


exports : FFI_Export FFI_OCaml "idrikernel.mli" []
exports = Fun HelloFunctor "Hello" End

main : OCaml_IO ()
main = pure ()
