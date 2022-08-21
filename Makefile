.PHONY: serve install

serve: install
	dune exec dominai

install:
	opam install -y . --deps-only