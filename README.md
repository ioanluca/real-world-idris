# Real World Idris

This is an experimental code generation backend
for [Idris](https://www.idris-lang.org/) that compiles
it to a slightly 
[modified version](https://github.com/ioanluca/malfunction)
of [Malfunction](https://github.com/stedolan/malfunction) that has
basic support for accessing identifiers outside the OCaml
standard library. 

This project also represents work in
progress towards allowing Idris and
[OCaml](http://www.ocaml.org/) to interoperate by 
implementing a ''Foreign Function Interface'' between the two languages
using Idris'
[New FFI](http://docs.idris-lang.org/en/latest/reference/ffi.html) 
(described in Edwin Brady's 
[paper](https://eb.host.cs.st-andrews.ac.uk/drafts/compile-idris.pdf)
and documented
[here](https://lenary.co.uk/publications/Elliott_BSc_Dissertation.pdf)).

## Installation 

#### Requirements
* [Stack](https://docs.haskellstack.org/en/stable/README/):
* OCaml `4.07.01+flambda`
* [Opam](https://opam.ocaml.org/)


Clone this repository: 

```
$ git clone --recurse-submodules https://github.com/ioanluca/real-world-idris.git
$ cd real-world-idris/
```

#### Using Stack

```
$ stack build 
```

or, if you want to install the binaries globally:

``` 
$ stack install 
$ stack install idris 
```

*All* `idris` *commands can be executed in the Stack
sandbox by prepending them with* `stack exec -- `

#### Install Malfunction using Opam
In the root of the repository:
```
$ cd malfunction/
$ opam install .
```

#### Install the `ocaml` Idris package
In the root of the repository:
```
$ cd lib/
$ idris --install ocaml.ipkg
```
This needs to be used in Idris programs
to access the FFI to OCaml. 

#### Install the `idrisobj` OCaml package
In the root of the repository:
```
$ cd rts/
$ opam install .  
```
This is a runtime support library. 
It is a wrapper around the OCaml
[Obj](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Obj.html) module.
The plan is to get rid of it and implement
the required functions from `Obj` using Idris primitives.

## Usage

```
$ idris Source.idr --codegen malfunction -p ocaml --cg-opt '-op ocamllib1 -op ocamllib2' -o a.out
$ ./a.out
```

`-op` is short for `--ocamlpackage` 

Alternatively, `%lib malfunction "ocamllib"` can be used in an Idris file to link an OCaml library.


## Examples 

Some test programs can be found 
in [test-idris](/test-idris/).

To compile the 
[graphics](/test-idris/graphics/) example:
```
$ cd test-idris/
$ cd graphics/
$ idris IdrCamlGrphics.idr --codegen malfunction -p ocaml --cg-opt '-op graphics' -o CamlGraphics.out
$ ./CamlGraphics.out
```

Note that the above example is linking the OCaml 
[graphics](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Graphics.html)
module using the `--cg-opt '-op graphics'` flags.

Alternatively, `graphics` could also be linked by having the 
following line in [IdrCamlGraphics.idr](/test-idris/graphics/IdrCamlGraphics.idr):
```
%lib malfunction "graphics"
```


## Benchmarking
Some example programs can be found in 
[benchmark](/benchmark/).

It seems to go pretty fast compared to the default C backend:
```    
$ idris pythag.idr -o pythag-idris
$ idris pythag.idr --codegen malfunction -o pythag-malfunction
    
$ time ./pythag-idris  > /dev/null
   
real    0m4.548s
user    0m4.528s
sys     0m0.016s
    
$ time ./pythag-malfunction  > /dev/null
    
real    0m0.654s
user    0m0.652s
sys     0m0.000s
```

Tested on:
* Ubuntu `16.04.4 LTS 64-bit`
* Intel® Core™ i5-2500 CPU @ 3.30GHz × 4 
* 8 GB RAM
* Idris `1.2.0`
* Malfunction `v0.2.1`
* OCaml `4.05.0+flambda`



## Todo
* [ ] Todo
* [ ] Todo
* [ ] Todo
* [ ] Todo
* [ ] Todo


## Contributing 
Contributions are welcome!