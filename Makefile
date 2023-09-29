.PHONY: dev game frontend
MAKEFLAGS += -j8

dev: game frontend

game:
	cd game && opam exec dune exec dominai

frontend:
	cd frontend && yarn dev