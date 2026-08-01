#!/usr/bin/env bash
set -euo pipefail

cleanup_all() {
  rm -rf .tmp-directive-*.??????
}
trap cleanup_all EXIT

run_fixture() {
  local name="$1"
  local expected="$2"
  local tmp_dir
  tmp_dir="$(mktemp -d ".tmp-directive-${name}.XXXXXX")"

  cleanup() {
    rm -rf "$tmp_dir"
  }
  trap cleanup RETURN

  mkdir -p "$tmp_dir/src"

  cat > "$tmp_dir/rescript.json" <<'JSON'
{
  "name": "resgraph-invalid-directive-test",
  "uncurried": true,
  "package-specs": {
    "in-source": true,
    "module": "esmodule",
    "suffix": ".mjs"
  },
  "sources": [
    {"dir": "../../src/res", "subdirs": true},
    {"dir": "src", "subdirs": true}
  ],
  "compiler-flags": ["-w -33-44"],
  "dependencies": ["@glennsl/rescript-fetch", "rescript-nodejs"]
}
JSON

  cat > "$tmp_dir/src/Query.res" <<'RES'
@gql.type
type query
RES

  cat > "$tmp_dir/src/ResGraphContext.res" <<'RES'
type t = unit
RES

  cat > "$tmp_dir/src/App.res" <<'RES'
@@warning("-32-101")

RES
  cat >> "$tmp_dir/src/App.res"

  (
    cd "$tmp_dir"
    ../node_modules/.bin/rescript
  )

  local output
  set +e
  output="$(../bin/dev/resgraph.exe generate-schema "$tmp_dir/src" "$tmp_dir/src/__generated__" true)"
  set -e

  if ! printf "%s" "$output" | grep -q '"status": "Error"'; then
    printf "Expected directive fixture %s to fail, but it succeeded.\n%s\n" "$name" "$output"
    exit 1
  fi

  if ! printf "%s" "$output" | grep -q "$expected"; then
    printf "Expected directive fixture %s to include diagnostic %s, got:\n%s\n" "$name" "$expected" "$output"
    exit 1
  fi
}

run_fixture "unknown" 'Directive `@missing` is not defined' <<'RES'
@gql.annotate({name: "missing"})
@gql.type
type broken = {@gql.field value: string}

@gql.field
let broken = (_: Query.query): broken => {value: "broken"}
RES

run_fixture "wrong-location" 'does not include `OBJECT`' <<'RES'
@gql.directive({locations: ["FIELD_DEFINITION"]})
type fieldOnly

@gql.annotate({name: "fieldOnly"})
@gql.type
type broken = {@gql.field value: string}

@gql.field
let broken = (_: Query.query): broken => {value: "broken"}
RES

run_fixture "missing-argument" 'requires argument `value`' <<'RES'
@gql.directive({locations: ["OBJECT"]})
type requiresValue = {value: int}

@gql.annotate({name: "requiresValue"})
@gql.type
type broken = {@gql.field value: string}

@gql.field
let broken = (_: Query.query): broken => {value: "broken"}
RES

run_fixture "wrong-argument-type" 'Invalid value for `@typed(value:)`' <<'RES'
@gql.directive({locations: ["OBJECT"]})
type typed = {value: int}

@gql.annotate({name: "typed", args: {value: "not-an-int"}})
@gql.type
type broken = {@gql.field value: string}

@gql.field
let broken = (_: Query.query): broken => {value: "broken"}
RES

run_fixture "not-repeatable" 'Directive `@once` is not repeatable' <<'RES'
@gql.directive({locations: ["OBJECT"]})
type once

@gql.annotate({name: "once"})
@gql.annotate({name: "once"})
@gql.type
type broken = {@gql.field value: string}

@gql.field
let broken = (_: Query.query): broken => {value: "broken"}
RES

run_fixture "invalid-location" 'is not a GraphQL directive location' <<'RES'
@gql.directive({locations: ["OBJECTISH"]})
type invalidLocation
RES

run_fixture "invalid-input-default" 'Invalid default for input field `Broken.count`' <<'RES'
@gql.inputObject
type broken = {
  @gql.default("not-an-int")
  count: int,
}
RES

run_fixture "deprecated-required-input" 'Required input field `Broken.value` cannot be deprecated without a default value' <<'RES'
@gql.inputObject
type broken = {
  @deprecated("Use the replacement field.")
  value: string,
}
RES

run_fixture "nonconstant-argument-default" 'Invalid default for resolver argument `Query.broken(count:)`' <<'RES'
@gql.field
let broken = (_: Query.query, ~count: int={
  let value = 1
  value
}) => count
RES

run_fixture "deprecated-required-argument" 'Required argument `Query.broken(value:)` cannot be deprecated without a default value' <<'RES'
@gql.field
let broken = (
  _: Query.query,
  @deprecated("Use the replacement argument.")
  ~value: string,
) => value
RES

run_fixture "argument-directive-location" 'does not include `ARGUMENT_DEFINITION`' <<'RES'
@gql.directive({locations: ["OBJECT"]})
type objectOnly

@gql.field
let broken = (
  _: Query.query,
  @gql.annotate({name: "objectOnly"})
  ~value: string,
) => value
RES

run_fixture "schema-directive-location" 'does not include `SCHEMA`' <<'RES'
@gql.directive({locations: ["OBJECT"]})
type objectOnly

@gql.annotate({name: "objectOnly"})
@gql.schema
type schemaMarker
RES

run_fixture "duplicate-schema-marker" 'Only one `@gql.schema` type is allowed' <<'RES'
@gql.schema
type firstSchemaMarker

@gql.schema
type secondSchemaMarker
RES

run_fixture "missing-schema-root" 'query root maps to `MissingQuery`' <<'RES'
@gql.schema({query: "MissingQuery"})
type schemaMarker
RES

run_fixture "invalid-schema-marker" 'schema marker must be declared as an abstract type' <<'RES'
@gql.schema
type schemaMarker = {value: string}
RES

run_fixture "root-shorthand-positional-argument" 'must take either `unit` followed by labelled arguments' <<'RES'
@gql.query
let broken = (value: string) => value
RES

run_fixture "root-shorthand-not-function" 'root-field annotation, but is not a function' <<'RES'
@gql.query
let broken = "value"
RES

run_fixture "duplicate-directive-annotation" 'Only one `@gql.directive` annotation is allowed' <<'RES'
@gql.directive({locations: ["OBJECT"]})
@gql.directive({locations: ["FIELD_DEFINITION"]})
type duplicateDirective
RES

run_fixture "invalid-name" 'is not a valid GraphQL name' <<'RES'
@as("invalid-name")
@gql.directive({locations: ["OBJECT"]})
type invalidName
RES

run_fixture "int-out-of-range" 'Invalid default for directive argument' <<'RES'
@gql.directive({locations: ["OBJECT"]})
type invalidInt = {
  @gql.default(2147483648)
  value: int,
}
RES
