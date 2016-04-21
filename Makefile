PHONY: build

OCB = ocamlbuild -use-ocamlfind

build: main.native

main.native: main.ml raz_simp.ml fingertree.ml
	$(OCB) main.native

run: main.native
	./main.native

clean:
	ocamlbuild -clean