SHELL = /bin/bash

build-resgraph-binary:
	rm -f bin/dev/resgraph.exe
	dune build --profile release
	cp _build/install/default/bin/resgraph bin/dev/resgraph.exe

build-resgraph-binary-dev:
	rm -f bin/dev/resgraph.exe
	dune build
	cp _build/install/default/bin/resgraph bin/dev/resgraph.exe

build-cli:
	npm run build

build-tests:
	make -C tests build

build: build-resgraph-binary build-cli build-tests

dce: build-resgraph-binary
	opam exec reanalyze.exe -- -dce-cmt _build -suppress vendor

format:
	dune build @fmt --auto-promote

test-resgraph-binary: build-resgraph-binary build-cli
	make -C tests test

test: test-resgraph-binary

clean:
	rm -f bin/dev/resgraph.exe
	dune clean
	make -C tests clean

checkformat:
	dune build @fmt

.DEFAULT_GOAL := build

.PHONY: build-resgraph-binary build-resgraph-binary-dev build-cli build-tests dce clean format test
