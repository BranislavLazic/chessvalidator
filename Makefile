APP=chessvalidator
VERSION := v0.1

.PHONY: clean test

all: clean test binaries

test:
	dune test

clean:
	rm -rf _build release

binaries:
	dune build
