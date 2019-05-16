idrikernel.cmxa: idrikernel.cmi idrikernel.cmx
	ocamlopt -g -a -o $@ idrikernel.cmx

idrikernel.cmx: idrikernel.cmi Idrikernel.idr
	stack exec -- idris --codegen malfunction Idrikernel.idr -i ../../lib/ -o idrikernel.cmx

idrikernel.cmi: idrikernel.mli
	ocamlfind ocamlc -package mirage-time-lwt -c idrikernel.mli

test: test.ml idrikernel.cmxa
	ocamlfind ocamlopt -package mirage-time-unix,idrisobj -thread -g -linkpkg -o test idrikernel.cmxa test.ml

install: idrikernel.cmxa
	ocamlfind install idrikernel META idrikernel.cmi idrikernel.cmxa idrikernel.cmx idrikernel.a idrikernel.o

remove:
	ocamlfind remove idrikernel

clean:
	rm -f *.a *.cmi *.cmx *.cmxa *.o *.ibc *~ *.mlf *.lang
	rm -f test
