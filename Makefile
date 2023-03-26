SHELL = /bin/bash

build-resgraph-binary:
	rm -f resgraph.exe
	dune build
	cp _build/install/default/bin/resgraph resgraph.exe

build-tests:
	make -C tests build

build: build-resgraph-binary build-tests

dce: build-resgraph-binary
	opam exec reanalyze.exe -- -dce-cmt _build -suppress vendor

format:
	dune build @fmt --auto-promote

test-resgraph-binary: build-resgraph-binary
	make -C tests test

test: test-resgraph-binary

clean:
	rm -f resgraph.exe
	dune clean
	make -C tests clean
	make -C reanalyze clean

checkformat:
	dune build @fmt

.DEFAULT_GOAL := build

.PHONY: build-resgraph-binary build-tests dce clean format test
