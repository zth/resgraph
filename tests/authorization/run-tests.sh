#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "$0")/../.." && pwd)"
rescript_bin="$root_dir/node_modules/.bin/rescript"
resgraph_bin="${RESGRAPH_BIN:-$root_dir/_build/default/src/ml/Cli.exe}"
tmp_dir="$(mktemp -d /tmp/resgraph-authorization-tests.XXXXXX)"
trap 'rm -rf "$tmp_dir"' EXIT

cd "$root_dir"

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
grep -F 'switch AliasResolvers.aliasedOutcome' "$tmp_dir/valid/ResGraphSchema.res" >/dev/null
grep -F 'switch await AliasResolvers.aliasedAsyncOutcome' \
  "$tmp_dir/valid/ResGraphSchema.res" >/dev/null
grep -F 'switch OutcomeNamed.computed' "$tmp_dir/valid/ResGraphSchema.res" >/dev/null
grep -F 'OutcomeDevice.computed(src)' "$tmp_dir/valid/ResGraphSchema.res" >/dev/null
if grep -F 'switch OutcomeDevice.computed' "$tmp_dir/valid/ResGraphSchema.res" >/dev/null; then
  echo "Concrete interface resolver override was treated as an outcome." >&2
  exit 1
fi
grep -F 'PlainNamed.computed(src)' "$tmp_dir/valid/ResGraphSchema.res" >/dev/null
if grep -F 'switch PlainNamed.computed' "$tmp_dir/valid/ResGraphSchema.res" >/dev/null; then
  echo "Inherited resolver used outcome metadata from a different interface." >&2
  exit 1
fi
grep -F 'Security.canFindUser(Obj.magic(src), ~args=authorizationArgs, ~ctx=ctx, ~info=info)' \
  "$tmp_dir/valid/ResGraphSchema.res" >/dev/null
grep -F 'ResGraph.Authorization.raiseError(Security.onForbidden(reason, ~ctx=ctx, ~info=info))' \
  "$tmp_dir/valid/ResGraphSchema.res" >/dev/null
grep -F 'switch Security.canReadNamed(Obj.magic(src)' "$tmp_dir/valid/ResGraphSchema.res" >/dev/null
grep -F 'switch Security.first' "$tmp_dir/valid/ResGraphSchema.res" >/dev/null
grep -F 'switch Security.Alias.second' "$tmp_dir/valid/ResGraphSchema.res" >/dev/null
diff -u "$root_dir/tests/authorization/valid/expected-authorization-manifest.json" \
  "$tmp_dir/valid/authorization-manifest.json"

mkdir -p "$tmp_dir/cache-bypass"
"$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/invalid/src" \
  "$tmp_dir/cache-bypass" false \
  >"$tmp_dir/cache-bypass-optional-result.json"
"$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/invalid/src" \
  "$tmp_dir/cache-bypass" false required - \
  "$tmp_dir/cache-bypass/authorization-manifest.json" \
  >"$tmp_dir/cache-bypass-required-result.json"
grep -F 'Field `Query.uncovered` has no authorization disposition.' \
  "$tmp_dir/cache-bypass-required-result.json" >/dev/null

"$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/invalid/src" \
  "$tmp_dir/invalid" false required - "$tmp_dir/valid/authorization-manifest.json" \
  >"$tmp_dir/invalid-result.json"

jq -e '.generatedBy == "resgraph" and .status == "generationFailed" and (.fields | length) == 0' \
  "$tmp_dir/valid/authorization-manifest.json" >/dev/null

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

cp "$root_dir/tests/authorization/valid/expected-authorization-manifest.json" \
  "$tmp_dir/valid/authorization-manifest.json"
if "$resgraph_bin" generate-schema \
  "$tmp_dir/not-a-project" "$tmp_dir/early-error" false required - \
  "$tmp_dir/valid/authorization-manifest.json" >/dev/null 2>/dev/null; then
  echo "Generation unexpectedly succeeded for a missing ReScript project." >&2
  exit 1
fi
jq -e '.generatedBy == "resgraph" and .status == "generationFailed" and (.fields | length) == 0' \
  "$tmp_dir/valid/authorization-manifest.json" >/dev/null

cp "$root_dir/tests/authorization/valid/expected-authorization-manifest.json" \
  "$tmp_dir/output-is-a-file"
cp "$root_dir/tests/authorization/valid/expected-authorization-manifest.json" \
  "$tmp_dir/late-failure-manifest.json"
if "$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/valid/src" "$tmp_dir/output-is-a-file" \
  false required Security.onForbidden "$tmp_dir/late-failure-manifest.json" \
  >/dev/null 2>/dev/null; then
  echo "Generation unexpectedly succeeded with an invalid output folder." >&2
  exit 1
fi
jq -e '.generatedBy == "resgraph" and .status == "generationFailed" and (.fields | length) == 0' \
  "$tmp_dir/late-failure-manifest.json" >/dev/null

cp "$root_dir/tests/authorization/config/resgraph.json" \
  "$tmp_dir/manifest-collision.json"
cp "$tmp_dir/manifest-collision.json" "$tmp_dir/manifest-collision.expected.json"
if "$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/valid/src" "$tmp_dir/collision-output" \
  false required Security.onForbidden "$tmp_dir/manifest-collision.json" \
  >/dev/null 2>/dev/null; then
  echo "Generation unexpectedly overwrote a non-manifest file." >&2
  exit 1
fi
cmp "$tmp_dir/manifest-collision.expected.json" "$tmp_dir/manifest-collision.json"

mkdir -p "$tmp_dir/artifact-output"
if "$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/valid/src" "$tmp_dir/artifact-output" \
  false required Security.onForbidden "$tmp_dir/artifact-output/ResGraphSchema.res" \
  >/dev/null 2>/dev/null; then
  echo "Generation unexpectedly accepted a manifest/schema path collision." >&2
  exit 1
fi
if [[ -e "$tmp_dir/artifact-output/ResGraphSchema.res" ]]; then
  echo "Manifest preparation wrote over a generated schema artifact path." >&2
  exit 1
fi

mkdir -p "$tmp_dir/interface-artifact-output"
if "$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/valid/src" \
  "$tmp_dir/interface-artifact-output" false required Security.onForbidden \
  "$tmp_dir/interface-artifact-output/interface_named.res" \
  >/dev/null 2>/dev/null; then
  echo "Generation unexpectedly accepted an interface/manifest path collision." >&2
  exit 1
fi
if [[ -e "$tmp_dir/interface-artifact-output/interface_named.res" ]]; then
  echo "Manifest preparation wrote over an interface artifact path." >&2
  exit 1
fi

state_file="$root_dir/tests/authorization/valid/lib/.resgraphState.marshal"
state_checksum_before="$(sha256sum "$state_file" | cut -d ' ' -f 1)"
if "$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/valid/src" "$tmp_dir/state-artifact-output" \
  false required Security.onForbidden "$state_file" \
  >/dev/null 2>"$tmp_dir/state-collision-error.txt"; then
  echo "Generation unexpectedly accepted a state/manifest path collision." >&2
  exit 1
fi
grep -F 'collides with a generated schema artifact' \
  "$tmp_dir/state-collision-error.txt" >/dev/null
state_checksum_after="$(sha256sum "$state_file" | cut -d ' ' -f 1)"
if [[ "$state_checksum_before" != "$state_checksum_after" ]]; then
  echo "Manifest preparation modified the compiler state artifact." >&2
  exit 1
fi

mkdir -p "$tmp_dir/cli-project/src" "$tmp_dir/cli-project/generated/schema"
cp "$root_dir/tests/authorization/config/resgraph.json" \
  "$tmp_dir/cli-project/resgraph.json"
cp "$root_dir/tests/authorization/config/resgraph.json" \
  "$tmp_dir/cli-project/generated/authorization-manifest.json"
cp "$tmp_dir/cli-project/generated/authorization-manifest.json" \
  "$tmp_dir/cli-project/generated/authorization-manifest.expected.json"
if (cd "$tmp_dir/cli-project" && node "$root_dir/cli/Cli.mjs" build) \
  >/dev/null 2>/dev/null; then
  echo "JavaScript CLI reported success after native generation failed." >&2
  exit 1
fi
cmp "$tmp_dir/cli-project/generated/authorization-manifest.expected.json" \
  "$tmp_dir/cli-project/generated/authorization-manifest.json"

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
