.PHONY: test check
.PHONY: timer

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

timer:
	OCAMLRUNPARAM=b dune exec timer/timer.exe

test:
	OCAMLRUNPARAM=b dune exec test/test.exe

cloc:
	cloc --by-file --include-lang=OCaml .

zip:
	rm -f restaurant.zip
	zip -r restaurant.zip . -x@exclude.lst

clean:
	dune clean
	rm -f restaurant.zip

doc:
	dune build @doc