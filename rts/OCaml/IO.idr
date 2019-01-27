module OCaml.IO

%default total
%access public export

unRaw : FFI_C.Raw a -> a
unRaw (MkRaw x) = x

||| Supported OCaml foreign types.
data OCamlTypes : Type -> Type where

  -- Primitive types
  OCamlInt      : OCamlTypes Int

FFI_OCaml : FFI
FFI_OCaml = MkFFI OCamlTypes String String