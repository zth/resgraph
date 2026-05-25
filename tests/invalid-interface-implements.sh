#!/usr/bin/env bash
set -euo pipefail

cleanup_all() {
  rm -rf .tmp-*.??????
}
trap cleanup_all EXIT

run_fixture() {
  local name="$1"
  local expected="$2"
  local tmp_dir
  tmp_dir="$(mktemp -d ".tmp-${name}.XXXXXX")"

  cleanup() {
    rm -rf "$tmp_dir"
  }
  trap cleanup RETURN

  mkdir -p "$tmp_dir/src"

  cat > "$tmp_dir/rescript.json" <<'JSON'
{
  "name": "resgraph-invalid-interface-implements-test",
  "uncurried": true,
  "package-specs": {
    "in-source": true,
    "module": "esmodule",
    "suffix": ".mjs"
  },
  "sources": [
    {
      "dir": "../../src/res",
      "subdirs": true
    },
    {
      "dir": "src",
      "subdirs": true
    }
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
@@warning("-32")

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
    printf "Expected fixture %s to fail, but it succeeded.\n%s\n" "$name" "$output"
    exit 1
  fi

  if ! printf "%s" "$output" | grep -q "$expected"; then
    printf "Expected fixture %s to include diagnostic %s, got:\n%s\n" "$name" "$expected" "$output"
    exit 1
  fi
}

run_fixture "missing-interface" "no @gql.interface named" <<'RES'
@gql.implements("Missing")
@gql.type
type broken = {
  @gql.field name: string,
}

@gql.field
let broken = (_: Query.query): broken => {name: "broken"}
RES

run_fixture "invalid-payload" '`@gql.implements` requires a string literal' <<'RES'
@gql.implements(1)
@gql.type
type broken = {
  @gql.field name: string,
}

@gql.field
let broken = (_: Query.query): broken => {name: "broken"}
RES

run_fixture "unsupported-target" 'can only be used on @gql.type or @gql.interface records' <<'RES'
@gql.interface
type named = {
  @gql.field name: string,
}

@gql.implements("Named")
@gql.inputObject
type brokenInput = {
  name: string,
}
RES

run_fixture "missing-field" 'missing field `name: String!`' <<'RES'
@gql.interface
type named = {
  @gql.field name: string,
}

@gql.implements("Named")
@gql.type
type broken = {
  @gql.field title: string,
}

@gql.field
let broken = (_: Query.query): broken => {title: "broken"}
RES

run_fixture "field-type-mismatch" 'field `name` has type `Int!` but the interface requires `String!`' <<'RES'
@gql.interface
type named = {
  @gql.field name: string,
}

@gql.implements("Named")
@gql.type
type broken = {
  @gql.field name: int,
}

@gql.field
let broken = (_: Query.query): broken => {name: 1}
RES

run_fixture "missing-argument" 'field `items` is missing argument `first`' <<'RES'
@gql.interface
type searchable = {
  @gql.field id: string,
}

@gql.field
let items = (_: searchable, ~first: int): string => Int.toString(first)

@gql.implements("Searchable")
@gql.type
type broken = {
  @gql.field id: string,
}

@gql.field
let items = (_: broken): string => "broken"

@gql.field
let broken = (_: Query.query): broken => {id: "1"}
RES

run_fixture "argument-type-mismatch" 'argument `first` on field `items` has type `String!` but the interface requires `Int!`' <<'RES'
@gql.interface
type searchable = {
  @gql.field id: string,
}

@gql.field
let items = (_: searchable, ~first: int): string => Int.toString(first)

@gql.implements("Searchable")
@gql.type
type broken = {
  @gql.field id: string,
}

@gql.field
let items = (_: broken, ~first: string): string => first

@gql.field
let broken = (_: Query.query): broken => {id: "1"}
RES

run_fixture "extra-required-argument" 'adds required argument `required`' <<'RES'
@gql.interface
type searchable = {
  @gql.field id: string,
}

@gql.field
let items = (_: searchable): string => "ok"

@gql.implements("Searchable")
@gql.type
type broken = {
  @gql.field id: string,
}

@gql.field
let items = (_: broken, ~required: string): string => required

@gql.field
let broken = (_: Query.query): broken => {id: "1"}
RES

run_fixture "interface-to-interface-missing-field" 'missing field `name: String!`' <<'RES'
@gql.interface
type named = {
  @gql.field name: string,
}

@gql.implements("Named")
@gql.interface
type brokenInterface = {
  @gql.field id: string,
}

@gql.implements("BrokenInterface")
@gql.type
type broken = {
  @gql.field id: string,
}

@gql.field
let broken = (_: Query.query): broken => {id: "1"}
RES

run_fixture "interface-cycle" 'cannot implement itself' <<'RES'
@gql.implements("B")
@gql.interface
type a = {
  @gql.field id: string,
}

@gql.implements("A")
@gql.interface
type b = {
  @gql.field id: string,
}

@gql.implements("A")
@gql.type
type broken = {
  @gql.field id: string,
}

@gql.field
let broken = (_: Query.query): broken => {id: "1"}
RES
