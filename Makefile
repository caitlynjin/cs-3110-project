.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop lib

start:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f restaurant.zip
	zip -r restaurant.zip . -x@exclude.lst

clean:
	dune clean
	rm -f restaurant.zip