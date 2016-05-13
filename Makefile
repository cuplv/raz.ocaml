PHONY: build

OCB = ocamlbuild -use-ocamlfind

build: main.native

main.native: main.ml raz_simp.ml fingertree.ml batEnum.ml batInnerIO.ml batInnerWeaktbl.ml batConcurrent.ml batRef.ml batOrd.ml batInterfaces.ml batList.ml batMap.ml batPrintf.ml batBuffer.ml batString.ml batReturn.ml batChar.ml batInt.ml batNumber.ml
	$(OCB) main.native

run: main.native
	./main.native

clean:
	ocamlbuild -clean