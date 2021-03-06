module OCaml.IO

%default total
%access public export

--------- The OCaml FFI

data OCamlRaw : Type -> Type where
  MkOCamlRaw : (x:t) -> OCamlRaw t
%used MkOCamlRaw x

data Module : List (String, Type) -> Type

val : String -> Type -> (String, Type)
val s t = (s,t)

data Abstr1 : Type -> Type

mutual
  data OCaml_IntTypes  : Type -> Type where
       OCaml_IntChar   : OCaml_IntTypes Char
       OCaml_IntNative : OCaml_IntTypes Int
       OCaml_IntBits32 : OCaml_IntTypes Bits32
       OCaml_IntBits64 : OCaml_IntTypes Bits64

  data OCaml_FnTypes : Type -> Type where
       OCaml_Fn     : OCaml_Types s -> OCaml_FnTypes t -> OCaml_FnTypes (s -> t)
       OCaml_FnIO   : OCaml_Types s -> OCaml_Types t -> OCaml_FnTypes (s -> IO' l t)
       OCaml_FnBase : OCaml_Types s -> OCaml_Types t -> OCaml_FnTypes (s -> t)

  data OCamlTypeList : List (String, Type) -> Type where
       Done : OCamlTypeList []
       Next : OCaml_Types a -> OCamlTypeList tys -> OCamlTypeList ((l, a) :: tys)

  data OCaml_Types : Type -> Type where
       OCaml_Str   : OCaml_Types String
       OCaml_Float : OCaml_Types Double
       OCaml_Bool  : OCaml_Types Bool
       OCaml_Abstr1: OCaml_Types (Abstr1 a)
       OCaml_Ptr   : OCaml_Types Ptr
       OCaml_Unit  : OCaml_Types ()
       OCaml_Any   : OCaml_Types (OCamlRaw a)
       OCaml_FnT   : OCaml_FnTypes a -> OCaml_Types a
       OCaml_Pair  : OCaml_Types a -> OCaml_Types b -> OCaml_Types (a, b)
       OCaml_List  : OCaml_Types a -> OCaml_Types (List a)
       OCaml_Maybe : OCaml_Types a -> OCaml_Types (Maybe a)
       OCaml_IntT  : OCaml_IntTypes i -> OCaml_Types i
       OCaml_Mod   : OCamlTypeList tys -> OCaml_Types (Module tys)

%error_reverse
FFI_OCaml : FFI
FFI_OCaml = MkFFI OCaml_Types String String

%error_reverse
OCaml_IO : Type -> Type
OCaml_IO = IO' FFI_OCaml

mutual
  -- Translates an OCaml-conventions function to an Idris-conventions
  -- one by inserting an additional dummy 'world' argument.
  --%inline
  fromOCamlFn : OCaml_FnTypes a -> a -> a
  fromOCamlFn (OCaml_Fn s t)     f = \x => fromOCamlFn t (f (toOCaml s x))
  fromOCamlFn (OCaml_FnIO s t)   f = \x => pure (fromOCaml t (believe_me (f (toOCaml s x))))
  fromOCamlFn (OCaml_FnBase s t) f = \x => fromOCaml t (f (toOCaml s x))

  -- %inline
  fromOCaml : OCaml_Types a -> a -> a
  fromOCaml OCaml_Str        s = s
  fromOCaml OCaml_Float      f = f
  fromOCaml OCaml_Bool       b = b
  fromOCaml OCaml_Ptr        p = p
  fromOCaml OCaml_Abstr1     x = x
  fromOCaml OCaml_Unit       u = u
  fromOCaml OCaml_Any        a = a
  fromOCaml (OCaml_FnT t)    f = fromOCamlFn t f
  fromOCaml (OCaml_Pair s t) p = (fromOCaml s (fst p), fromOCaml t (snd p))
  fromOCaml (OCaml_List s)   l = map (fromOCaml s) l
  fromOCaml (OCaml_Maybe s)  m = map (fromOCaml s) m
  fromOCaml (OCaml_IntT _)   i = i
  fromOCaml (OCaml_Mod ts)   m = m

  --%inline
  toOCamlFn : OCaml_FnTypes a -> a -> a
  toOCamlFn (OCaml_Fn s t)     f = \x => toOCamlFn t (f (fromOCaml s x))
  toOCamlFn (OCaml_FnIO s t)   f = \x => believe_me (toOCaml t (unsafePerformIO (f (fromOCaml s x))))
  toOCamlFn (OCaml_FnBase s t) f = \x => toOCaml t (f (fromOCaml s x))

  -- %inline
  toOCaml : OCaml_Types a -> a -> a
  toOCaml OCaml_Str        s = s
  toOCaml OCaml_Float      f = f
  toOCaml OCaml_Bool       b = b
  toOCaml OCaml_Ptr        p = p
  toOCaml OCaml_Abstr1     x = x
  toOCaml OCaml_Unit       u = u
  toOCaml OCaml_Any        a = a
  toOCaml (OCaml_FnT t)    f = toOCamlFn t f
  toOCaml (OCaml_Pair s t) p = (toOCaml s (fst p), toOCaml t (snd p))
  toOCaml (OCaml_List s)   l = map (toOCaml s) l
  toOCaml (OCaml_Maybe s)  m = map (toOCaml s) m
  toOCaml (OCaml_IntT _)   i = i
  toOCaml (OCaml_Mod ts)   m = m

%inline
fromOCamlFTy : FTy FFI_OCaml xs ty -> ty -> ty
fromOCamlFTy (FRet t)   f = do x <- f; pure (fromOCaml t x)
fromOCamlFTy (FFun s t) f = \x => fromOCamlFTy t (f (toOCaml s x))

%inline
ocamlCall : (fname : String) -> (ty : Type) ->
          {auto fty : FTy FFI_OCaml [] ty} -> ty
ocamlCall fname ty {fty} = fromOCamlFTy fty (foreign FFI_OCaml fname ty)

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

-- Some specific functions from the Stdlib module

print_endline : String -> OCaml_IO ()
print_endline s =
  ocamlCall "Stdlib.print_endline" (String -> OCaml_IO ()) s



-- Modules

data StrItem : String -> Type -> Type where
  Let : (s : String) -> t -> StrItem s t

data Values : List (String, Type) -> Type where
  Nil  : Values []
  (::) : StrItem s t -> Values tys -> Values ((s, t) :: tys)

assocIdx : String -> Int -> List (String, Type) -> Maybe (Int, Type)
assocIdx k i [] = Nothing
assocIdx k i ((k',t)::tys) = if k == k' then Just (i,t) else assocIdx k (i+1) tys

infixl 1 #

%inline
(#) : Module tys -> (nm : String) ->
      {auto ok : assocIdx nm 0 tys = Just (i, a)} ->
      {auto p : OCaml_Types a} ->
      {auto q : OCamlTypeList tys} ->
      a
(#) {tys} {a} {i} m nm = unsafePerformIO $
 ocamlCall "Idrisobj.field" (Module tys -> Int -> OCaml_IO a) m i

%inline
struct : Values tys -> {auto p : OCamlTypeList tys} -> Module tys
struct {tys} vs {p} = unsafePerformIO (go vs p 0) where
  %inline
  go : Values tys2 -> OCamlTypeList tys2 -> Int -> OCaml_IO (Module tys)
  go {tys2 = []} Nil Done n =
   ocamlCall "Idrisobj.new_block" (Int -> Int -> OCaml_IO (Module tys)) 0 n
  go {tys2 = (_, ty) :: tys2} (Let _ v :: vs) (Next t q) n = do
     m <- go vs q (n + 1)
     ocamlCall "Idrisobj.set_field" (Module tys -> Int -> ty -> OCaml_IO ()) m n v
     pure m
