import OCaml.IO

orderedType : Type -> Type
orderedType t = Module [ val "compare" (t -> t -> Int) ]

setSig : Type -> Type -> Type
setSig elt t = Module [ val "empty" t
                      , val "is_empty" (t -> Bool)
                      , val "mem" (elt -> t -> Bool)
                      , val "add" (elt -> t -> t)
                      ]

makeset : {auto p : OCaml_Types elt} ->
          orderedType elt ->
          (t : Type ** p : OCaml_Types t ** setSig elt t)
makeset {elt} ordered =
  MkDPair Ptr
  (MkDPair OCaml_Ptr
    (unsafePerformIO $
     ocamlCall "Stdlib.Set.Make" (orderedType elt -> OCaml_IO (setSig elt Ptr)) ordered))

IntOrdered : orderedType Int
IntOrdered = struct [ Let "compare" (\x => \y => x - y) ]

main : IO ()
main = do
  let MkDPair s (MkDPair _ ops) = makeset IntOrdered
  let set = ops#"empty"
  let set1 = (ops#"add") 45 set
  if (ops#"is_empty") set then putStrLn "'set' is empty"
                          else putStrLn "'set' is not empty"
  if (ops#"is_empty") set1 then putStrLn "'set1' is empty"
                           else putStrLn "'set1' is not empty"
