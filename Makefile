
main: main.ml
	ocamlfind ocamlopt -package tsdl -linkpkg -o $@ $^

clean:
	-rm main *.cmx *.cmi *.o
