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
(
  cd "$root_dir/tests/authorization/baseline"
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
if [[ "$(grep -c 'switch Security.canLoadSelection' "$tmp_dir/valid/ResGraphSchema.res")" -ne 1 ]]; then
  echo "Ancestor policy was not emitted exactly once at its declared field." >&2
  exit 1
fi
diff -u "$root_dir/tests/authorization/valid/expected-authorization-manifest.json" \
  "$tmp_dir/valid/authorization-manifest.json"
jq -e '
  . as $manifest |
  ([$manifest.fields[] | select(.disposition == "authorizedByAncestor")] | length) == 9 and
  ($manifest.fields[] | select(.coordinate == "OutcomePayload.value") |
    .ancestorBoundaries == [{
      "coordinate": "Query.outcomePayload",
      "kind": "resolverOutcome",
      "resolverOutcome": {"async": false}
    }]) and
  ($manifest.fields[] | select(.coordinate == "SelectionConnection.label") |
    .disposition == "public" and .byAncestor == null) and
  ($manifest.fields[] | select(.coordinate == "SharedProtected.value") |
    [.ancestorBoundaries[].coordinate] == [
      "Query.firstShared",
      "Query.secondShared"
    ])' \
  "$tmp_dir/valid/authorization-manifest.json" >/dev/null

mkdir -p "$tmp_dir/manifest-upgrade"
cp "$root_dir/tests/authorization/valid/expected-authorization-manifest.json" \
  "$tmp_dir/manifest-upgrade/authorization-manifest.json"
sed -i.bak 's/"version": 2/"version": 1/' \
  "$tmp_dir/manifest-upgrade/authorization-manifest.json"
"$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/valid/src" \
  "$tmp_dir/manifest-upgrade" false required Security.onForbidden \
  "$tmp_dir/manifest-upgrade/authorization-manifest.json" \
  >"$tmp_dir/manifest-upgrade-result.json"
jq -e '.generatedBy == "resgraph" and .version == 2 and .status == "success"' \
  "$tmp_dir/manifest-upgrade/authorization-manifest.json" >/dev/null

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
grep -F '`@gql.authorize.byAncestor` cannot be combined with `@gql.authorize(...)`' \
  "$tmp_dir/invalid-result.json" >/dev/null
grep -F 'Only one `@gql.authorize.byAncestor` annotation is allowed' \
  "$tmp_dir/invalid-result.json" >/dev/null
grep -F '`@gql.authorize.byAncestor` requires a reason with at least 3' \
  "$tmp_dir/invalid-result.json" >/dev/null
grep -F 'Field `Query.noAncestor` uses `@gql.authorize.byAncestor`' \
  "$tmp_dir/invalid-result.json" >/dev/null
grep -F 'Field `SharedSelection.value` uses `@gql.authorize.byAncestor`' \
  "$tmp_dir/invalid-result.json" >/dev/null
grep -F 'Field `SubscriptionEvent.value` uses `@gql.authorize.byAncestor`' \
  "$tmp_dir/invalid-result.json" >/dev/null
grep -F '`@gql.authorize.byAncestor` is currently supported on concrete object types' \
  "$tmp_dir/invalid-result.json" >/dev/null
grep -F 'Required authorization coverage does not support subscription field `Subscription.events` yet.' \
  "$tmp_dir/invalid-result.json" >/dev/null

mkdir -p "$tmp_dir/baseline-output"
baseline_path="$tmp_dir/authorization-baseline.json"
baseline_manifest_path="$tmp_dir/baseline-manifest.json"
"$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/baseline/src" "$tmp_dir/baseline-output" \
  false baseline - "$baseline_manifest_path" "$baseline_path" \
  >"$tmp_dir/baseline-create-result.json"

jq -e '
  .generatedBy == "resgraph" and
  .kind == "authorizationBaseline" and
  .version == 1 and
  .gaps == [
    {"coordinate":"Mutation.outcomeOnly","kind":"mutationPreResolverPolicy"},
    {"coordinate":"Query.legacy","kind":"uncoveredField"},
    {"coordinate":"Query.newField","kind":"uncoveredField"},
    {"coordinate":"Subscription.events","kind":"unsupportedSubscription"}
  ]' "$baseline_path" >/dev/null
baseline_checksum_before="$(sha256sum "$baseline_path" | cut -d ' ' -f 1)"

"$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/baseline/src" "$tmp_dir/baseline-output" \
  false baseline - "$baseline_manifest_path" "$baseline_path" \
  >"$tmp_dir/baseline-recreate-result.json"
baseline_checksum_after="$(sha256sum "$baseline_path" | cut -d ' ' -f 1)"
if [[ "$baseline_checksum_before" != "$baseline_checksum_after" ]]; then
  echo "Authorization baseline generation was not deterministic." >&2
  exit 1
fi

"$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/baseline/src" "$tmp_dir/baseline-output" \
  false required - "$baseline_manifest_path" "$baseline_path" \
  >"$tmp_dir/baseline-required-result.json"
grep -F '"status": "Success"' "$tmp_dir/baseline-required-result.json" >/dev/null
jq -e '[.fields[] | select(.disposition == "baseline")] | length == 4' \
  "$baseline_manifest_path" >/dev/null

cp "$baseline_path" "$tmp_dir/baseline-before-collision.json"
if "$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/baseline/src" "$tmp_dir/baseline-output" \
  false required - "$baseline_path" "$baseline_path" \
  >/dev/null 2>/dev/null; then
  echo "Required mode accepted a manifest/baseline path collision." >&2
  exit 1
fi
cmp "$tmp_dir/baseline-before-collision.json" "$baseline_path"

if "$resgraph_bin" generate-schema \
  "$tmp_dir/missing-project/src" "$tmp_dir/missing-project/output" \
  false required - "$baseline_path" "$baseline_path" \
  >/dev/null 2>/dev/null; then
  echo "Early project collection accepted a manifest/baseline collision." >&2
  exit 1
fi
cmp "$tmp_dir/baseline-before-collision.json" "$baseline_path"

required_artifact_output="$tmp_dir/required-artifact-output"
mkdir -p "$required_artifact_output"
cp "$baseline_path" "$required_artifact_output/ResGraphSchema.res"
cp "$baseline_path" "$tmp_dir/schema-baseline.expected.json"
if "$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/baseline/src" "$required_artifact_output" \
  false required - - "$required_artifact_output/ResGraphSchema.res" \
  >/dev/null 2>/dev/null; then
  echo "Required mode accepted a baseline/schema artifact collision." >&2
  exit 1
fi
cmp "$tmp_dir/schema-baseline.expected.json" \
  "$required_artifact_output/ResGraphSchema.res"

symlink_artifact_output="$tmp_dir/symlink-artifact-output"
symlink_baseline_path="$tmp_dir/symlink-authorization-baseline.json"
mkdir -p "$symlink_artifact_output"
cp "$baseline_path" "$symlink_baseline_path"
cp "$baseline_path" "$tmp_dir/symlink-baseline.expected.json"
ln -s "$symlink_baseline_path" "$symlink_artifact_output/ResGraphSchema.res"
if "$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/baseline/src" "$symlink_artifact_output" \
  false required - - "$symlink_baseline_path" \
  >/dev/null 2>/dev/null; then
  echo "Required mode accepted a symlinked baseline/schema collision." >&2
  exit 1
fi
cmp "$tmp_dir/symlink-baseline.expected.json" "$symlink_baseline_path"
if [[ ! -L "$symlink_artifact_output/ResGraphSchema.res" ]]; then
  echo "Schema generation replaced the colliding symlink." >&2
  exit 1
fi

dangling_artifact_output="$tmp_dir/dangling-artifact-output"
dangling_baseline_path="$tmp_dir/dangling-authorization-baseline.json"
mkdir -p "$dangling_artifact_output"
ln -s "$dangling_artifact_output/ResGraphSchema.res" "$dangling_baseline_path"
if "$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/baseline/src" "$dangling_artifact_output" \
  false baseline - - "$dangling_baseline_path" \
  >/dev/null 2>/dev/null; then
  echo "Baseline generation accepted a dangling symlink to the schema output." >&2
  exit 1
fi
if [[ ! -L "$dangling_baseline_path" || \
      -e "$dangling_artifact_output/ResGraphSchema.res" ]]; then
  echo "Baseline generation modified a dangling artifact collision." >&2
  exit 1
fi

cp "$baseline_path" "$required_artifact_output/interface_legacy.res"
cp "$baseline_path" "$tmp_dir/interface-baseline.expected.json"
if "$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/baseline/src" "$required_artifact_output" \
  false required - - "$required_artifact_output/interface_legacy.res" \
  >/dev/null 2>/dev/null; then
  echo "Required mode accepted a baseline/interface artifact collision." >&2
  exit 1
fi
cmp "$tmp_dir/interface-baseline.expected.json" \
  "$required_artifact_output/interface_legacy.res"

state_collision_project="$tmp_dir/required-state-collision-project"
mkdir -p "$state_collision_project/src" "$state_collision_project/output"
cp "$root_dir/tests/authorization/baseline/rescript.json" \
  "$state_collision_project/rescript.json"
cp "$root_dir/tests/authorization/baseline/src/"*.res \
  "$state_collision_project/src/"
(
  cd "$state_collision_project"
  "$rescript_bin"
)
state_baseline_path="$state_collision_project/lib/.resgraphState.marshal"
cp "$baseline_path" "$state_baseline_path"
cp "$baseline_path" "$tmp_dir/state-baseline.expected.json"
if "$resgraph_bin" generate-schema \
  "$state_collision_project/src" "$state_collision_project/output" \
  false required - - "$state_baseline_path" \
  >/dev/null 2>/dev/null; then
  echo "Required mode accepted a baseline/state artifact collision." >&2
  exit 1
fi
cmp "$tmp_dir/state-baseline.expected.json" "$state_baseline_path"

"$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/baseline/src" "$tmp_dir/baseline-output" \
  false required - - "$tmp_dir/missing-baseline.json" \
  >"$tmp_dir/missing-baseline-result.json"
jq -e '.status == "Error" and (.errors | length) == 1' \
  "$tmp_dir/missing-baseline-result.json" >/dev/null
grep -F 'Run `resgraph authorization baseline` to create it.' \
  "$tmp_dir/missing-baseline-result.json" >/dev/null

printf '%s\n' '{"generatedBy":"resgraph","kind":"authorizationBaseline"' \
  >"$tmp_dir/malformed-baseline.json"
"$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/baseline/src" "$tmp_dir/baseline-output" \
  false required - - "$tmp_dir/malformed-baseline.json" \
  >"$tmp_dir/malformed-baseline-result.json"
jq -e '.status == "Error" and (.errors | length) == 1' \
  "$tmp_dir/malformed-baseline-result.json" >/dev/null
grep -F 'expected valid JSON' "$tmp_dir/malformed-baseline-result.json" >/dev/null

jq 'del(.gaps[] | select(.coordinate == "Query.newField"))' \
  "$baseline_path" >"$tmp_dir/missing-gap-baseline.json"
"$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/baseline/src" "$tmp_dir/baseline-output" \
  false required - - "$tmp_dir/missing-gap-baseline.json" \
  >"$tmp_dir/missing-gap-result.json"
grep -F 'Field `Query.newField` has no authorization disposition.' \
  "$tmp_dir/missing-gap-result.json" >/dev/null

jq '.gaps += [{"coordinate":"Query.removed","kind":"uncoveredField"}]' \
  "$baseline_path" >"$tmp_dir/stale-baseline.json"
"$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/baseline/src" "$tmp_dir/baseline-output" \
  false required - - "$tmp_dir/stale-baseline.json" \
  >"$tmp_dir/stale-baseline-result.json"
grep -F 'Authorization baseline entry `Query.removed` (`uncoveredField`) is stale.' \
  "$tmp_dir/stale-baseline-result.json" >/dev/null

printf '%s\n' '{"ownedBy":"application"}' >"$tmp_dir/not-generated-baseline.json"
cp "$tmp_dir/not-generated-baseline.json" "$tmp_dir/not-generated-baseline.expected.json"
if "$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/baseline/src" "$tmp_dir/baseline-output" \
  false baseline - - "$tmp_dir/not-generated-baseline.json" \
  >/dev/null 2>/dev/null; then
  echo "Baseline generation unexpectedly overwrote an application-owned file." >&2
  exit 1
fi
cmp "$tmp_dir/not-generated-baseline.expected.json" \
  "$tmp_dir/not-generated-baseline.json"

printf '%s\n' 'not a directory' >"$tmp_dir/baseline-parent"
if "$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/baseline/src" "$tmp_dir/baseline-output" \
  false baseline - "$baseline_manifest_path" \
  "$tmp_dir/baseline-parent/authorization-baseline.json" \
  >/dev/null 2>/dev/null; then
  echo "Baseline generation unexpectedly succeeded after a late write failure." >&2
  exit 1
fi
jq -e '.generatedBy == "resgraph" and .status == "generationFailed"' \
  "$baseline_manifest_path" >/dev/null

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

printf '%s\n' '{"generatedBy":"resgraph","ownedBy":"application"}' \
  >"$tmp_dir/marker-only-manifest.json"
cp "$tmp_dir/marker-only-manifest.json" \
  "$tmp_dir/marker-only-manifest.expected.json"
if "$resgraph_bin" generate-schema \
  "$root_dir/tests/authorization/valid/src" "$tmp_dir/marker-only-output" \
  false required Security.onForbidden "$tmp_dir/marker-only-manifest.json" \
  >/dev/null 2>/dev/null; then
  echo "Generation unexpectedly trusted a marker-only application file." >&2
  exit 1
fi
cmp "$tmp_dir/marker-only-manifest.expected.json" \
  "$tmp_dir/marker-only-manifest.json"

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

cli_baseline_project="$tmp_dir/cli-baseline-project"
mkdir -p "$cli_baseline_project/src" "$cli_baseline_project/generated/schema"
cp "$root_dir/tests/authorization/baseline/rescript.json" \
  "$cli_baseline_project/rescript.json"
cp "$root_dir/tests/authorization/baseline/src/"*.res \
  "$cli_baseline_project/src/"
(
  cd "$cli_baseline_project"
  "$rescript_bin"
)
node --input-type=module -e \
  'import fs from "node:fs";
   import path from "node:path";
   const project = process.argv[1];
   fs.writeFileSync(path.join(project, "resgraph.json"), JSON.stringify({
     src: "./src",
     outputFolder: "./generated/schema",
     authorization: {
       mode: "required",
       manifestPath: "./generated/authorization-manifest.json",
       baselinePath: "./generated/authorization-baseline.json"
     }
   }, null, 2) + "\n");' "$cli_baseline_project"
(
  cd "$cli_baseline_project"
  node "$root_dir/cli/Cli.mjs" authorization baseline
  node "$root_dir/cli/Cli.mjs" build
) >"$tmp_dir/cli-baseline-output.txt" 2>&1
grep -F 'Authorization baseline written to' "$tmp_dir/cli-baseline-output.txt" >/dev/null
grep -F 'Build succeeded' "$tmp_dir/cli-baseline-output.txt" >/dev/null
warning_count="$(grep -Fc 'Authorization baseline active:' "$tmp_dir/cli-baseline-output.txt")"
if [[ "$warning_count" -ne 2 ]]; then
  echo "Expected baseline warnings after baseline generation and required build." >&2
  exit 1
fi
grep -F 'are not protected by required authorization coverage.' \
  "$tmp_dir/cli-baseline-output.txt" >/dev/null
jq -e '.kind == "authorizationBaseline" and (.gaps | length) == 4' \
  "$cli_baseline_project/generated/authorization-baseline.json" >/dev/null

node --input-type=module -e \
  'import fs from "node:fs";
   import path from "node:path";
   const project = process.argv[1];
   fs.writeFileSync(path.join(project, "resgraph.json"), JSON.stringify({
     defaultSchema: "secure",
     schemas: {
       secure: {
         projectRoot: ".",
         include: ["src"],
         outputFolder: "./generated/schema",
         authorization: {
           mode: "required",
           manifestPath: "./generated/authorization-manifest.json",
           baselinePath: "./generated/authorization-baseline.json"
         }
       }
     }
   }, null, 2) + "\n");' "$cli_baseline_project"
(
  cd "$cli_baseline_project"
  node "$root_dir/cli/Cli.mjs" authorization baseline secure
  node "$root_dir/cli/Cli.mjs" build secure
) >"$tmp_dir/named-cli-baseline-output.txt" 2>&1
grep -F 'Authorization baseline written to' \
  "$tmp_dir/named-cli-baseline-output.txt" >/dev/null
grep -F '[secure] Build succeeded' "$tmp_dir/named-cli-baseline-output.txt" >/dev/null
test -f "$cli_baseline_project/generated/schema/SecureSchema.res"
test ! -e "$cli_baseline_project/generated/schema/ResGraphSchema.res"
jq -e '.kind == "authorizationBaseline" and (.gaps | length) == 4' \
  "$cli_baseline_project/generated/authorization-baseline.json" >/dev/null

node --input-type=module -e \
  'import path from "node:path";
   import {readConfigFromDir} from "./cli/Utils.mjs";
   const configDir = path.resolve("tests/authorization/config");
   const result = readConfigFromDir(configDir);
   const config = result.TAG === "Ok" ? result._0 : undefined;
   if (config?.src !== path.resolve(configDir, "src") ||
       config?.outputFolder !== path.resolve(configDir, "generated/schema") ||
       config?.authorization?.manifestPath !== path.resolve(configDir, "generated/authorization-manifest.json") ||
       config?.authorization?.baselinePath !== path.resolve(configDir, "generated/authorization-baseline.json")) process.exit(1)'

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
