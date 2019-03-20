



# OCaml types
what do with 
- [x] structs -  blocks
- [ ] vectors - vecs
- [x] mutable refs 
- [x] tuples of length > 2 blocks 
  - [ ] consider using indexed vectors
- [x] modules?? blocks (add type)
    - (do I need smth in the FFI descriptor 
    instead of plain `String`?) |
    - `js` and `python` kinda create a `%include` directive
- [x] functors are functions from blocks to blocks

### use ocamly any (`Raw`) and `Ptr` for them?

- `Raw` smth Idris understands and OCaml doesn't
- `Ptr` smth OCaml understands and Idris doesn't

### passing functions works but not if they do IO, they work `unsafely` 
### ask if you understand the function types?

```ocaml
    module HTTPS () () () () : sig val start : Pclock.t -> DATA.t -> KEYS.t -> Http.t -> unit end

    (lambda ($0 $1 $2 $3)) (block 0 (lambda ()) 
```
Also, here generate a an mli with just the start function in it?



# Idris exports
### as `.ml`? 
probably needed, generate mli from idk man

# The lib for Mirage
works for simple examples.
### what about `functors`? 

- [x] thinking about a `--mirage` flag so that it builds a libary
- [x] also need a way to pass packages that need to be linked with the `.cmx` for the `.cma` 
    - [x] use codegen params




# Report

### How much content about what i did at the internship? that's technically another thing; so I could not describe how say `case` expressions are compiled because that was then; however, I implemented some more primitivies and floats and the AST and compiled foreign calls etc. **the line is kinda blurry**

## Structure 

- cover
- abstract
- declaration
- contents
    - Introduction
        - Dependent Types
        - Idris 
            - TDD
            - interactive editing?
            - theorem proving
        - OCaml
        - Mirage
        - Objectives
        - Contributions ?
        - Outline 
    - Related Work
        - Other backends  (C, Py, Js, Erlang, Old Mlf)
        - my backend 
        - idris compiler
        - Malfunction 
        - other projects compiling to OCaml (haven't really heard of any)
        - The Rise of library operating systems
            - Unikernels
            - Mirage
    - Design
        - Compiler backends for idris
        - The IO and the FFI in idris
        - Idris Libraries
        - OCaml Libraries
    - Implementation 
        - Idris IRs
        - Idris primitives
        - Malfunction code generation
        - OCaml Types vs Idris types
        - tool chain
    - Evaluation 
        - Example programs
        - Were my objectives met?
        - What I didnt manage to do
    - Conclusion 
        - Future work
    - Appendi
        
## talk about

- [ ] idris 
- [ ] malfunction 
- [ ] mirage 
- [ ] ocaml
- [ ] dependent types
- [ ] my other backend
- [ ] idris compiler inner works
- [ ] how stuff is compiled
- [ ] the ffi
- [ ] benchmarks (related work?)
- [ ] idris slow idris not enough libraries idris not popular (problem descripiton)
- [ ] describe the monad used, avoid saying design pattern, use applicatives etc
- [ ] how I broke the AST 
- [ ] how you modified what, added to Malfunction for ex

check commit history to see what happened chaos

make latex report structure summarizing chapters sectins ETC

## testing and validation 
- [ ]  manual
- [ ]  other tests
- [ ]  semantics of the program is preserver, i.e. test against the idris 

## do 
- [ ] requirements all kinds
- [ ] self evaluation, what worked and what didn't

### results and eval is stuff you done actually
### summary and conslusions is stuff you haven't really done like you can kinda brag about it or where would it be next year

## in apendix 
- [ ] code, the actual ffi etc
- [ ] documentation how to run the program
- [ ] document the API, idris maybe haskell as well?

## Style

### spell check
- [ ] `detex` 
- [ ] unix wc

### code snippets 
- [ ] listings change to other monospace

### biblio style
- [ ] use the ones that say the name


# write programs in:
- [ ] graphics
- [ ] base64
- [ ] co-http

# future work
- [ ] gadts
- [ ] modules
- [ ] modules user friendly




## Misc
### `latex` package for inserting code?
listings but change font



# maybe build with dune with custom rule





# Monday

- [ ] need to generate the `mli`
    - [ ] was thinking to use exports, idk if it's correct to use it like that
    - [ ] have a function in `mlf` that creates modules/functors 

- [ ] and then write everything in Idris
    - need some start data types for that I think
    - and to change the FFI descriptor to something more powerful than `String`

module is a list of OCamlTypes?

- [ ] type annotations
- [ ] add string
- [ ] make set field and get field 



