#!/usr/bin/env bash
set -euo pipefail

tmp_dir="$(mktemp -d .tmp-schema-marker.XXXXXX)"
cleanup() {
  rm -rf "$tmp_dir"
}
trap cleanup EXIT

mkdir -p "$tmp_dir/src/__generated__"

cat >"$tmp_dir/rescript.json" <<'JSON'
{
  "name": "resgraph-schema-marker-test",
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

cat >"$tmp_dir/src/ResGraphContext.res" <<'RES'
type t = unit
RES

cat >"$tmp_dir/src/App.res" <<'RES'
/** Identifies the schema in runtime transforms. */
@gql.directive({locations: ["SCHEMA"]})
type schemaName = {value: string}

@gql.type
type rootQuery = {
  @gql.field
  version: string,
}

@gql.field
let greeting = (_: rootQuery) => "hello"

/** A schema with a custom query root. */
@gql.annotate({name: "schemaName", args: {value: "custom"}})
@gql.schema({query: "RootQuery"})
type schemaMarker
RES

(
  cd "$tmp_dir"
  ../node_modules/.bin/rescript
)

../bin/dev/resgraph.exe generate-schema \
  "$tmp_dir/src" "$tmp_dir/src/__generated__" true >/dev/null

grep -Fq 'description: "A schema with a custom query root.",' \
  "$tmp_dir/src/__generated__/ResGraphSchema.res"
grep -Fq 'query: get_RootQuery(),' \
  "$tmp_dir/src/__generated__/ResGraphSchema.res"
grep -Fq 'schema @schemaName(value: "custom") {' \
  "$tmp_dir/src/__generated__/schema.graphql"
grep -Fq 'query: RootQuery' \
  "$tmp_dir/src/__generated__/schema.graphql"

printf '%s\n' '✅ Schema descriptions, directives, and custom roots work.'
