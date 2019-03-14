module Main 

import OCaml.IO

do_fopen : String -> String -> IO Ptr
do_fopen f m
   = foreign FFI_C "fileOpen" (String -> String -> IO Ptr) f m

r : Int
r = unsafePerformIO $
   foreign FFI_OCaml "min_int" (OCaml_IO Int)


lineOfAfile : OCaml_IO Unit
lineOfAfile = do 
     min_int <- ocamlCall "Pervasives.min_int" (OCaml_IO Int)
     prim_write (show min_int)
     ic <- ocamlCall "Pervasives.open_in" (String -> OCaml_IO Ptr) "README.md"
     str <- ocamlCall "Pervasives.input_line" (Ptr -> OCaml_IO String) ic
     -- xs <- foreign FFI_OCaml "List.map" ((Int -> Int) -> List Int -> OCaml_IO ( List Int)) (+1) [1,2,3]
     -- prim_write $ show xs
     prim_write str
     {-foreign FFI_OCaml "print_endline" (String -> OCaml_IO Unit) -} 
     pure ()

main : OCaml_IO ()
main = do
    lineOfAfile