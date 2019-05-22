test_server: IdrisCohttp.idr
	stack exec -- idris --codegen malfunction IdrisCohttp.idr -i ../../lib -o test_server

clean:
	rm -f *.a *.cmi *.cmx *.cmxa *.o *.ibc *~ *.mlf *.lang
	rm -f test_server
