import OCaml.IO

%lib malfunction "duration"
%lib malfunction "lwt"

Lwt : Type -> Type
Lwt _ = Ptr

Int64 : Type
Int64 = Ptr

lwtUnit : OCaml_IO Ptr
lwtUnit = ocamlCall "Lwt.return_unit" (OCaml_IO Ptr)

lwtReturn : () -> OCaml_IO Ptr
lwtReturn = ocamlCall "Lwt.return" (() -> OCaml_IO Ptr)

of_sec : Int -> Int64
of_sec n =
    unsafePerformIO $ ocamlCall "Duration.of_sec" (Int -> OCaml_IO Int64) n

-- lwtBind : Lwt a -> (a -> OCaml_IO (Lwt b)) -> OCaml_IO (Lwt b)
lwtBind : Ptr -> (Ptr -> OCaml_IO Ptr) -> OCaml_IO Ptr
lwtBind p f =
  ocamlCall "Lwt.bind" (Ptr -> OCamlFn (Ptr -> OCaml_IO Ptr) -> OCaml_IO Ptr) p (MkOCamlFn f)

{-
start : TimeSig -> OCaml_IO Ptr
start timeMod = loop 4
    where loop : Int -> OCaml_IO Ptr
          loop 0 = lwtUnit
          loop n = do
            putStrLn' "Idris Unikernel Hello!"
            (MkOCamlFn sleep) <- modGet 0 timeMod
            let lwtThread = sleep (of_sec 1)
            lwtThread `lwtBind` (\ _ => loop $ n - 1)
-}

fixOCamlFn : OCaml_FnTypes a -> a -> a
fixOCamlFn (OCaml_Fn s t)   f = \x => fixOCamlFn t (f x)
fixOCamlFn (OCaml_FnIO t)   f = pure (believe_me f)
fixOCamlFn (OCaml_FnBase t) f = f

ocamlFn : {auto p : OCaml_FnTypes a} -> OCamlFn a -> a
ocamlFn {p} (MkOCamlFn f) = fixOCamlFn p f

unfixOCamlFn : OCaml_FnTypes a -> a -> a
unfixOCamlFn (OCaml_Fn s t)   f = \x => fixOCamlFn t (f x)
unfixOCamlFn (OCaml_FnIO t)   f = believe_me (unsafePerformIO f)
unfixOCamlFn (OCaml_FnBase t) f = f

mkOCamlFn : {auto p : OCaml_FnTypes a} -> a -> OCamlFn a
mkOCamlFn {p} f = MkOCamlFn (unfixOCamlFn p f)


TimeSig : Type
TimeSig = OCamlModule [ OCamlFn (Int64 -> OCaml_IO (Lwt ())) ] -- 0: sleep_ns

HelloSig : Type
HelloSig = TimeSig ->
   OCamlModule [OCamlFn (Ptr -> OCaml_IO (Lwt ()))]

Hello : HelloSig
Hello time = mkModule (Step (mkOCamlFn start) Stop)
  where
     sleep : Int64 -> OCaml_IO (Lwt ())
     sleep = ocamlFn $ modGet 0 time

     loop : Int -> OCaml_IO Ptr
     loop 0 = lwtUnit
     loop n = do
       putStrLn' "Idris Unikernel Hello!"
       lwtThread <- sleep (of_sec 1)
       lwtThread `lwtBind` (\ _ => loop $ n - 1)

     start : Ptr -> OCaml_IO (Lwt ())
     start _ = loop 4


exports : FFI_Export FFI_OCaml "idrikernel.mli" []
exports = Fun Hello "Hello" End

main : OCaml_IO ()
main = pure ()
