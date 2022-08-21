.PHONY: serve install

serve: install
	opam exec dune exec dominai

install:
	opam install -y . --deps-only