module Main 

import OCaml.IO

f : String -> (String, Maybe Double)
f x = (x ++ x, Just (1.3 + 1.3))


main : OCaml_IO ()
main = do
     min_int <- ocamlCall "Pervasives.min_int" (OCaml_IO Int)
     printLn min_int
     ic <- ocamlCall "Pervasives.open_in" (String -> OCaml_IO Ptr) "Foo.idr"
     str <- ocamlCall "Pervasives.input_line" (Ptr -> OCaml_IO String) ic
     l <- ocamlCall "List.length" (List Int -> OCaml_IO Int) [1,2,3]
     ll <- ocamlCall "List.flatten" ((List (List Int)) -> OCaml_IO (List Int)) [[1,2,3], [9,10,11]]
     printLn l
     printLn ll
     printLn str
     ys <- ocamlCall "List.map" ((Int -> Int) -> List Int -> OCaml_IO (List Int)) ((+1)) [1..10]
     printLn ys
     zs <- ocamlCall "List.map" ((String -> (String, Maybe Double)) -> List String -> OCaml_IO (List (String, Maybe Double))) f ["hello", "world"]
     printLn zs
     ocamlCall "List.iter" ((Double -> OCaml_IO ()) -> List Double -> OCaml_IO ())
        printLn'
        [1.1,213.321]
     ocamlCall "List.iter2"
        ((Int -> Int -> OCaml_IO ()) -> List Int -> List Int -> OCaml_IO ())
        (\a => \b => printLn' (a + b))
        [1..10]
        [11..20]
     ocamlCall "List.init"
       (Int -> (Int -> OCaml_IO ()) -> OCaml_IO (List ()))
       4
       (\x => printLn' x)
     pure ()

      --   printLn is
