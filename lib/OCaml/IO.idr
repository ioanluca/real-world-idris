module OCaml.IO 

%default total
%access public export

--------- The OCaml FFI

data OCamlRaw : Type -> Type where
  MkOCamlRaw : (x:t) -> OCamlRaw t
%used MkOCamlRaw x

data OCamlModule : List Type -> Type

mutual
  data OCamlFn : Type -> Type where
       MkOCamlFn : (x : t) -> OCamlFn t

  data OCaml_IntTypes  : Type -> Type where
       OCaml_IntChar   : OCaml_IntTypes Char
       OCaml_IntNative : OCaml_IntTypes Int
       OCaml_IntBits32 : OCaml_IntTypes Bits32
       OCaml_IntBits64 : OCaml_IntTypes Bits64

  data OCaml_FnTypes : Type -> Type where
       OCaml_Fn     : OCaml_Types s -> OCaml_FnTypes t -> OCaml_FnTypes (s -> t)
       OCaml_FnIO   : OCaml_Types t -> OCaml_FnTypes (IO' l t)
       OCaml_FnBase : OCaml_Types t -> OCaml_FnTypes t

  data OCamlTypeList : List Type -> Type where
       Done : OCamlTypeList []
       Next : OCaml_Types a -> OCamlTypeList tys -> OCamlTypeList (a :: tys)

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
       OCaml_Mod   : OCamlTypeList tys -> OCaml_Types (OCamlModule tys)

-- Tell erasure analysis not to erase the argument. Needs to be outside the
-- mutual block, since directives are done on the first pass and in the first
-- pass we only have 'OCamlFn' and not the constructor.
%used MkOCamlFn x

%error_reverse
FFI_OCaml : FFI
FFI_OCaml = MkFFI OCaml_Types String String

%error_reverse
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

-- Modules

data Values : List Type -> Type where
  Stop : Values []
  Step : t -> Values tys -> Values (t :: tys)

modGet : (i : Nat) -> OCamlModule tys ->
         {auto ok : index' i tys = Just a} ->
         {auto p : OCaml_Types a} ->
         {auto q : OCamlTypeList tys} ->
         a
modGet {tys = tys} {a = a} i m = unsafePerformIO $
 ocamlCall "Idrisobj.field" (OCamlModule tys -> Int -> OCaml_IO a) m (cast i)


mkModule : Values tys -> {auto p : OCamlTypeList tys} -> OCamlModule tys
mkModule {tys = tys} vs {p = p} = unsafePerformIO (go vs p 0) where
  go : Values tys2 -> OCamlTypeList tys2 ->
       Int -> OCaml_IO (OCamlModule tys)
  go {tys2 = []} Stop Done n =
   ocamlCall "Idrisobj.new_block" (Int -> Int -> OCaml_IO (OCamlModule tys)) 0 n
  go {tys2 = ty :: tys2} (Step v vs) (Next x q) n = do
     m <- go vs q (n + 1)
     ocamlCall "Idrisobj.set_field" 
          (OCamlModule tys -> Int -> ty -> OCaml_IO ()) m n v
     pure m

fixOCamlFn : OCaml_FnTypes a -> a -> a
fixOCamlFn (OCaml_Fn s t)   f = \x => fixOCamlFn t (f x)
fixOCamlFn (OCaml_FnIO t)   f = pure (believe_me f)
fixOCamlFn (OCaml_FnBase t) f = f

unOCamlFn : {auto p : OCaml_FnTypes a} -> OCamlFn a -> a
unOCamlFn {p} (MkOCamlFn f) = fixOCamlFn p f

unfixOCamlFn : OCaml_FnTypes a -> a -> a
unfixOCamlFn (OCaml_Fn s t)   f = \x => unfixOCamlFn t (f x)
unfixOCamlFn (OCaml_FnIO t)   f = believe_me (unsafePerformIO f)
unfixOCamlFn (OCaml_FnBase t) f = f

mkOCamlFn : {auto p : OCaml_FnTypes a} -> a -> OCamlFn a
mkOCamlFn {p} f = MkOCamlFn (unfixOCamlFn p f)
