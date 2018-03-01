.PHONY: run build clean

OCB = ocamlbuild -use-ocamlfind

run: build
	mkdir solutions solutions/all solutions/bests
	./main.byte || true
	mv solutions solutions-$(shell date +%Y-%m-%d-%H-%M-%S)

build:
	$(OCB) main.byte

clean:
	$(OCB) -clean
