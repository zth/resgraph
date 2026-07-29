#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "$0")/../.." && pwd)"
rescript_bin="$root_dir/node_modules/.bin/rescript"
resgraph_bin="${RESGRAPH_BIN:-$root_dir/_build/default/src/ml/Cli.exe}"
tmp_dir="$(mktemp -d /tmp/resgraph-authorization-tests.XXXXXX)"
trap 'rm -rf "$tmp_dir"' EXIT

if [[ ! -x "$resgraph_bin" ]]; then
  echo "Build the native resgraph CLI first or set RESGRAPH_BIN." >&2
  exit 1
fi

(
  cd "$root_dir/tests/authorization/valid"
  "$rescript_bin"
)
(
  cd "$root_dir/tests/authorization/invalid"
  "$rescript_bin"
)

mkdir -p "$tmp_dir/valid" "$tmp_dir/invalid"
"$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/valid/src" \
  "$tmp_dir/valid" false required Security.onForbidden \
  "$tmp_dir/valid/authorization-manifest.json" \
  >"$tmp_dir/valid-result.json"

grep -F '"status": "Success"' "$tmp_dir/valid-result.json" >/dev/null
grep -F 'let authorizationArgs = %raw(`{}`)' "$tmp_dir/valid/ResGraphSchema.res" >/dev/null
grep -F 'switch await Security.canLoadAsync' "$tmp_dir/valid/ResGraphSchema.res" >/dev/null
grep -F 'switch await Query.asyncOutcome' "$tmp_dir/valid/ResGraphSchema.res" >/dev/null
grep -F 'switch OutcomeNamed.computed' "$tmp_dir/valid/ResGraphSchema.res" >/dev/null
grep -F 'OutcomeDevice.computed(src)' "$tmp_dir/valid/ResGraphSchema.res" >/dev/null
if grep -F 'switch OutcomeDevice.computed' "$tmp_dir/valid/ResGraphSchema.res" >/dev/null; then
  echo "Concrete interface resolver override was treated as an outcome." >&2
  exit 1
fi
grep -F 'Security.canFindUser(Obj.magic(src), ~args=authorizationArgs, ~ctx, ~info)' \
  "$tmp_dir/valid/ResGraphSchema.res" >/dev/null
grep -F 'ResGraph.Authorization.raiseError(Security.onForbidden(reason, ~ctx, ~info))' \
  "$tmp_dir/valid/ResGraphSchema.res" >/dev/null
grep -F 'switch Security.canReadNamed(Obj.magic(src)' "$tmp_dir/valid/ResGraphSchema.res" >/dev/null
grep -F 'switch Security.first' "$tmp_dir/valid/ResGraphSchema.res" >/dev/null
grep -F 'switch Security.Nested.second' "$tmp_dir/valid/ResGraphSchema.res" >/dev/null
diff -u "$root_dir/tests/authorization/valid/expected-authorization-manifest.json" \
  "$tmp_dir/valid/authorization-manifest.json"

"$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/invalid/src" \
  "$tmp_dir/invalid" false required - "$tmp_dir/valid/authorization-manifest.json" \
  >"$tmp_dir/invalid-result.json"

if [[ -e "$tmp_dir/valid/authorization-manifest.json" ]]; then
  echo "Stale authorization manifest survived a failed generation." >&2
  exit 1
fi

grep -F 'Field `Query.uncovered` has no authorization disposition.' \
  "$tmp_dir/invalid-result.json" >/dev/null
grep -F 'Could not resolve authorization function `Missing.policy`.' \
  "$tmp_dir/invalid-result.json" >/dev/null
grep -F 'must declare the mandatory `~args` polymorphic object' \
  "$tmp_dir/invalid-result.json" >/dev/null
grep -F 'requests unavailable field argument `missing`' \
  "$tmp_dir/invalid-result.json" >/dev/null
grep -F 'must return `ResGraph.Authorization.outcome' \
  "$tmp_dir/invalid-result.json" >/dev/null
grep -F 'Public coverage cannot be combined' "$tmp_dir/invalid-result.json" >/dev/null
grep -F 'Mutation field `Mutation.outcomeOnly` requires at least one pre-resolver' \
  "$tmp_dir/invalid-result.json" >/dev/null
grep -F 'Only one `@gql.public` annotation is allowed per field.' \
  "$tmp_dir/invalid-result.json" >/dev/null
grep -F 'requires a reason with at least 3 non-whitespace characters' \
  "$tmp_dir/invalid-result.json" >/dev/null
grep -F '`@gql.public` can only be used on output fields or resolver functions' \
  "$tmp_dir/invalid-result.json" >/dev/null
grep -F 'has source type `Mutation`, but it is applied to `Query`' \
  "$tmp_dir/invalid-result.json" >/dev/null
grep -F 'must declare `~args` as a ReScript polymorphic object' \
  "$tmp_dir/invalid-result.json" >/dev/null
grep -F 'has invalid `~ctx`' "$tmp_dir/invalid-result.json" >/dev/null
grep -F 'Required authorization coverage does not support subscriptions yet.' \
  "$tmp_dir/invalid-result.json" >/dev/null

node --input-type=module -e \
  'import path from "node:path";
   import {readConfigFromDir} from "./cli/Utils.mjs";
   const configDir = path.resolve("tests/authorization/config");
   const result = readConfigFromDir(configDir);
   const config = result.TAG === "Ok" ? result._0 : undefined;
   if (config?.src !== path.resolve(configDir, "src") ||
       config?.outputFolder !== path.resolve(configDir, "generated/schema") ||
       config?.authorization?.manifestPath !== path.resolve(configDir, "generated/authorization-manifest.json")) process.exit(1)'

node --input-type=module -e \
  'import {Authorization} from "./src/res/ResGraph.mjs";
   try { Authorization.raiseForbidden("internal") }
   catch (error) {
     if (error.message !== "Forbidden" || error.extensions?.code !== "FORBIDDEN") process.exit(1)
   }
   const custom = Authorization.makeError("Not authorized", "CUSTOM_FORBIDDEN");
   try { Authorization.raiseError(custom) }
   catch (error) {
     if (error.message !== "Not authorized" || error.extensions?.code !== "CUSTOM_FORBIDDEN") process.exit(1)
   }'

echo "Authorization fixtures passed."
