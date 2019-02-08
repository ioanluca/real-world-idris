module Main 

%default total
%access public export

--------- The OCaml FFI


-- Supported OCaml foreign types
mutual
  public export
  data OCamlFn : Type -> Type where
       MkOCamlFn : (x : t) -> OCamlFn t

  public export
  data OCaml_IntTypes  : Type -> Type where
       OCaml_IntChar   : OCaml_IntTypes Char
       OCaml_IntNative : OCaml_IntTypes Int

  public export
  data OCaml_FnTypes : Type -> Type where
       OCaml_Fn     : OCaml_Types s -> OCaml_FnTypes t -> OCaml_FnTypes (s -> t)
       OCaml_FnIO   : OCaml_Types t -> OCaml_FnTypes (IO' l t)
       OCaml_FnBase : OCaml_Types t -> OCaml_FnTypes t

  public export
  data OCaml_Types : Type -> Type where
       OCaml_Str   : OCaml_Types String
       OCaml_Float : OCaml_Types Double
       OCaml_Bool  : OCaml_Types Bool
       OCaml_Int   : OCaml_Types Int
       OCaml_Ptr   : OCaml_Types Ptr
       OCaml_Unit  : OCaml_Types ()
       OCaml_FnT   : OCaml_FnTypes a -> OCaml_Types (OCamlFn a)
     --   OCaml_IntT  : OCaml_IntTypes i -> OCaml_Types i

-- Tell erasure analysis not to erase the argument. Needs to be outside the
-- mutual block, since directives are done on the first pass and in the first
-- pass we only have 'OCamlFn' and not the constructor.
%used MkOCamlFn x

||| The JavaScript FFI. The strings naming functions in this API are
||| JavaScript code snippets, into which the arguments are substituted
||| for the placeholders `%0`, `%1`, etc.
%error_reverse
public export
FFI_OCaml : FFI
FFI_OCaml = MkFFI OCaml_Types String String

%error_reverse
public export
OCaml_IO : Type -> Type
OCaml_IO = IO' FFI_OCaml



do_fopen : String -> String -> IO Ptr
do_fopen f m
   = foreign FFI_C "fileOpen" (String -> String -> IO Ptr) f m

r : Int
r = unsafePerformIO $
   foreign FFI_OCaml "min_int" (OCaml_IO Int)


lineOfAfile : OCaml_IO Unit
lineOfAfile = do --unsafePerformIO $ do
     min_int <- foreign FFI_OCaml "min_int" (OCaml_IO Int)
     prim_write (show min_int)
     ic <- foreign FFI_OCaml "open_in" (String -> OCaml_IO Ptr) "test.mlf"
     str <- foreign FFI_OCaml "input_line" (Ptr -> OCaml_IO String) ic
     prim_write str
     {-foreign FFI_OCaml "print_endline" (String -> OCaml_IO Unit) -} 
     return ()

main : OCaml_IO ()
main = do
    lineOfAfile