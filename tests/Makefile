SHELL = /bin/bash

node_modules/.bin/rescript:
	npm ci

build: node_modules/.bin/rescript
	node_modules/.bin/rescript
	@output=$$(../bin/dev/resgraph.exe generate-schema ./src ./src/__generated__ true); \
	printf "%s\n" "$$output"; \
	if printf "%s" "$$output" | grep -q '"status": "Error"'; then \
		exit 1; \
	fi
	node_modules/.bin/rescript

test: build
	./test.sh
	authorization/run-tests.sh

clean:
	rm -rf node_modules lib

.DEFAULT_GOAL := test

.PHONY: clean test
