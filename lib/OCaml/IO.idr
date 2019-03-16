module OCaml.IO 

%default total
%access public export

--------- The OCaml FFI

data OCamlRaw : Type -> Type where
  MkOCamlRaw : (x:t) -> OCamlRaw t
%used MkOCamlRaw x

-- Supported OCaml foreign types
mutual
  public export
  data OCamlFn : Type -> Type where
       MkOCamlFn : (x : t) -> OCamlFn t

  public export
  data OCaml_IntTypes  : Type -> Type where
       OCaml_IntChar   : OCaml_IntTypes Char
       OCaml_IntNative : OCaml_IntTypes Int
       OCaml_IntBits32 : OCaml_IntTypes Bits32
       OCaml_IntBits64 : OCaml_IntTypes Bits64

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
       OCaml_Ptr   : OCaml_Types Ptr
       OCaml_Unit  : OCaml_Types ()
       OCaml_Any   : OCaml_Types (OCamlRaw a)
       OCaml_FnT   : OCaml_FnTypes a -> OCaml_Types (OCamlFn a)
       OCaml_Pair  : OCaml_Types a -> OCaml_Types b -> OCaml_Types (a, b)
       OCaml_List  : OCaml_Types a -> OCaml_Types (List a)
       OCaml_Maybe : OCaml_Types a -> OCaml_Types (Maybe a)
       OCaml_IntT  : OCaml_IntTypes i -> OCaml_Types i

-- Tell erasure analysis not to erase the argument. Needs to be outside the
-- mutual block, since directives are done on the first pass and in the first
-- pass we only have 'OCamlFn' and not the constructor.
%used MkOCamlFn x

%error_reverse
public export
FFI_OCaml : FFI
FFI_OCaml = MkFFI OCaml_Types String String

%error_reverse
public export
OCaml_IO : Type -> Type
OCaml_IO = IO' FFI_OCaml

%inline
ocamlCall : (fname : String) -> (ty : Type) ->
          {auto fty : FTy FFI_OCaml [] ty} -> ty
ocamlCall fname ty = foreign FFI_OCaml fname ty

printLn : Show a => a -> OCaml_IO ()
printLn = printLn'

putStrLn : String -> OCaml_IO ()
putStrLn = putStrLn'

print : Show a => a -> OCaml_IO ()
print = print'

putStr : String -> OCaml_IO ()
putStr = putStr'

getLine : OCaml_IO String
getLine = getLine'