idriscohttp.cmxa: idriscohttp.cmi idriscohttp.cmx
	ocamlopt -g -a -o $@ idriscohttp.cmx

idriscohttp.cmx: idriscohttp.cmi IdrisCohttp.idr
	stack exec -- idris --codegen malfunction IdrisCohttp.idr -i ../../lib/ -o idriscohttp.cmx

idriscohttp.cmi: idriscohttp.mli
	ocamlfind ocamlc -package cohttp,cohttp-lwt idriscohttp.mli

test: test.ml idriscohttp.cmxa
	ocamlfind ocamlopt -package cohttp,cohttp-lwt,uri,cohttp-lwt-unix -thread -g -linkpkg -o test idriscohttp.cmxa test.ml

install: idriscohttp.cmxa
	ocamlfind install idriscohttp META idriscohttp.cmi idriscohttp.cmxa idriscohttp.cmx idriscohttp.a idriscohttp.o

remove:
	ocamlfind remove idriscohttp

clean:
	rm -f *.a *.cmi *.cmx *.cmxa *.o *.ibc *~ *.mlf *.lang
	rm -f test
