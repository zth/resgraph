#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd "$(dirname "$0")" && pwd)
repo_dir=$(cd "$script_dir/.." && pwd)
fixture_dir="$script_dir/multi-schema"
rescript_bin="$script_dir/node_modules/.bin/rescript"
cli="$repo_dir/dist/Cli.mjs"

assert_contains() {
  local file=$1
  local expected=$2
  if ! grep -Fq "$expected" "$file"; then
    printf 'Expected %s to contain: %s\n' "$file" "$expected" >&2
    exit 1
  fi
}

assert_not_contains() {
  local file=$1
  local unexpected=$2
  if grep -Fq "$unexpected" "$file"; then
    printf 'Expected %s not to contain: %s\n' "$file" "$unexpected" >&2
    exit 1
  fi
}

watch_output=""
capture_watch_output() {
  local working_dir=$1
  local expected=$2
  shift 2
  local watch_log
  watch_log=$(mktemp /tmp/resgraph-watch.XXXXXX)
  (cd "$working_dir" && exec node "$cli" watch "$@") >"$watch_log" 2>&1 &
  local watch_pid=$!
  local watch_ready=false
  for _ in {1..50}; do
    if grep -Fq "$expected" "$watch_log"; then
      watch_ready=true
      break
    fi
    sleep 0.1
  done
  kill "$watch_pid" 2>/dev/null || true
  wait "$watch_pid" 2>/dev/null || true
  watch_output=$(<"$watch_log")
  rm -f "$watch_log"
  if [[ "$watch_ready" != true ]]; then
    printf '%s\n' "$watch_output" >&2
    exit 1
  fi
}

if [[ ! -e "$script_dir/node_modules/resgraph" ]]; then
  ln -s ../.. "$script_dir/node_modules/resgraph"
fi
if [[ ! -e "$fixture_dir/node_modules" ]]; then
  ln -s ../node_modules "$fixture_dir/node_modules"
fi
if [[ ! -e "$fixture_dir/package-a/node_modules" ]]; then
  ln -s ../../node_modules "$fixture_dir/package-a/node_modules"
fi
if [[ ! -e "$fixture_dir/package-b/node_modules" ]]; then
  ln -s ../../node_modules "$fixture_dir/package-b/node_modules"
fi

rm -f "$fixture_dir/src/generated/broken/BrokenSchema.res" "$fixture_dir/src/generated/broken/BrokenSchema.resi"
rm -f "$fixture_dir/src/generated/empty/EmptySchema.res" "$fixture_dir/src/generated/empty/EmptySchema.resi"
rm -f "$fixture_dir/uncompiled/src/generated/UncompiledSchema.res" "$fixture_dir/uncompiled/src/generated/UncompiledSchema.resi"
rm -rf "$fixture_dir/uncompiled/lib"

(cd "$fixture_dir" && "$rescript_bin" && node "$cli" build && "$rescript_bin")

public_sdl="$fixture_dir/src/generated/public/schema.graphql"
admin_sdl="$fixture_dir/src/generated/admin/schema.graphql"
assert_contains "$public_sdl" 'publicValue: String!'
assert_contains "$public_sdl" 'sharedItem: SharedItem!'
assert_not_contains "$public_sdl" 'adminValue'
assert_not_contains "$public_sdl" 'experimentalValue'
assert_contains "$admin_sdl" 'adminValue: Int!'
assert_contains "$admin_sdl" 'sharedItem: SharedItem!'
assert_not_contains "$admin_sdl" 'publicValue'

test -f "$fixture_dir/src/generated/public/PublicSchema.res"
test -f "$fixture_dir/src/generated/admin/AdminSchema.res"
test -f "$fixture_dir/src/generated/public/PublicSchema__Interface_entity.res"
test -f "$fixture_dir/src/generated/admin/AdminSchema__Interface_entity.res"
test ! -e "$fixture_dir/src/generated/public/ResGraphSchema.res"
assert_contains "$fixture_dir/src/generated/public/PublicSchema.resi" 'ResGraph.schema<PublicContext.context>'
assert_contains "$fixture_dir/src/generated/admin/AdminSchema.resi" 'ResGraph.schema<AdminContext.context>'
test -f "$fixture_dir/lib/resgraph/public.state.marshal"
test -f "$fixture_dir/lib/resgraph/admin.state.marshal"
test -f "$fixture_dir/lib/resgraph/public.incremental-cache"
test -f "$fixture_dir/lib/resgraph/admin.incremental-cache"

(cd "$fixture_dir" && node "$cli" build public >/dev/null)
(cd "$fixture_dir" && node "$cli" build admin >/dev/null)
public_cache_output=$(cd "$fixture_dir" && RESGRAPH_INCREMENTAL_DEBUG=1 node "$cli" build public 2>&1)
admin_cache_output=$(cd "$fixture_dir" && RESGRAPH_INCREMENTAL_DEBUG=1 node "$cli" build admin 2>&1)
[[ "$public_cache_output" == *'Incremental cache hit'* ]]
[[ "$admin_cache_output" == *'Incremental cache hit'* ]]

config_backup=$(mktemp /tmp/resgraph-config.XXXXXX)
cp "$fixture_dir/resgraph.json" "$config_backup"
restore_config() {
  cp "$config_backup" "$fixture_dir/resgraph.json"
  rm -f "$config_backup"
}
trap restore_config EXIT
cp "$fixture_dir/resgraph.admin-module-changed.json" "$fixture_dir/resgraph.json"
(cd "$fixture_dir" && node "$cli" build public >/dev/null)
test -f "$fixture_dir/src/generated/admin/AdminSchema.res"
test -f "$fixture_dir/src/generated/admin/AdminSchema.resi"
test -f "$fixture_dir/src/generated/admin/AdminSchema__Interface_entity.res"
test ! -e "$fixture_dir/src/generated/admin/ChangedAdminSchema.res"
restore_config
trap - EXIT
(cd "$fixture_dir" && node "$cli" build admin >/dev/null)

transfer_fixture="$fixture_dir/ownership-transfer"
transfer_config_backup=$(mktemp /tmp/resgraph-transfer-config.XXXXXX)
cp "$transfer_fixture/resgraph.json" "$transfer_config_backup"
restore_transfer_fixture() {
  cp "$transfer_config_backup" "$transfer_fixture/resgraph.json"
  rm -f "$transfer_config_backup"
  rm -rf "$transfer_fixture/lib"
  find "$fixture_dir/src/generated/transfer-a" -type f ! -name .gitkeep -delete
  find "$fixture_dir/src/generated/transfer-b" -type f ! -name .gitkeep -delete
}
trap restore_transfer_fixture EXIT
(cd "$transfer_fixture" && node "$cli" build >/dev/null)
test -f "$fixture_dir/src/generated/transfer-a/FirstSchema.res"
test -f "$fixture_dir/src/generated/transfer-b/SecondSchema.res"
cp "$transfer_fixture/resgraph.transferred.json" "$transfer_fixture/resgraph.json"
(cd "$transfer_fixture" && node "$cli" build first >/dev/null)
test -f "$fixture_dir/src/generated/transfer-a/SecondSchema.res"
test ! -e "$fixture_dir/src/generated/transfer-a/FirstSchema.res"
test ! -e "$fixture_dir/src/generated/transfer-b/SecondSchema.res"
test ! -e "$fixture_dir/src/generated/transfer-b/ThirdSchema.res"
restore_transfer_fixture
trap - EXIT

config_backup=$(mktemp /tmp/resgraph-config.XXXXXX)
cp "$fixture_dir/resgraph.json" "$config_backup"
trap restore_config EXIT
cp "$fixture_dir/resgraph.public-no-sdl.json" "$fixture_dir/resgraph.json"
rm -rf "$fixture_dir/lib/resgraph"
(cd "$fixture_dir" && node "$cli" build public >/dev/null)
test ! -e "$fixture_dir/src/generated/public/schema.graphql"
test -f "$fixture_dir/src/generated/admin/schema.graphql"
printf 'type UserOwned { id: ID! }\n' >"$fixture_dir/src/generated/public/schema.graphql"
(cd "$fixture_dir" && node "$cli" build public >/dev/null)
assert_contains "$fixture_dir/src/generated/public/schema.graphql" 'type UserOwned'
rm -f "$fixture_dir/src/generated/public/schema.graphql"
restore_config
trap - EXIT
(cd "$fixture_dir" && node "$cli" build >/dev/null)
test -f "$fixture_dir/src/generated/public/schema.graphql"

removed_schema_backup=$(mktemp -d /tmp/resgraph-removed-schema.XXXXXX)
cp "$fixture_dir/resgraph.json" "$removed_schema_backup/resgraph.json"
for generated_file in \
  AdminSchema.res \
  AdminSchema.resi \
  AdminSchema__Interface_entity.res \
  schema.graphql; do
  cp "$fixture_dir/src/generated/admin/$generated_file" \
    "$removed_schema_backup/$generated_file"
done
restore_removed_schema() {
  cp "$removed_schema_backup/resgraph.json" "$fixture_dir/resgraph.json"
  for generated_file in \
    AdminSchema.res \
    AdminSchema.resi \
    AdminSchema__Interface_entity.res \
    schema.graphql; do
    cp "$removed_schema_backup/$generated_file" \
      "$fixture_dir/src/generated/admin/$generated_file"
  done
  rm -f "$fixture_dir/src/generated/admin/AdminSchema__Interface_custom.res"
  rm -rf "$removed_schema_backup"
}
trap restore_removed_schema EXIT
printf 'let preserved = true\n' \
  >"$fixture_dir/src/generated/admin/AdminSchema__Interface_custom.res"
cp "$fixture_dir/resgraph.public-only.json" "$fixture_dir/resgraph.json"
(cd "$fixture_dir" && node "$cli" build public >/dev/null)
test ! -e "$fixture_dir/src/generated/admin/AdminSchema.res"
test ! -e "$fixture_dir/src/generated/admin/AdminSchema.resi"
test ! -e "$fixture_dir/src/generated/admin/AdminSchema__Interface_entity.res"
test ! -e "$fixture_dir/src/generated/admin/schema.graphql"
test -f "$fixture_dir/src/generated/admin/AdminSchema__Interface_custom.res"
restore_removed_schema
trap - EXIT
(cd "$fixture_dir" && node "$cli" build admin >/dev/null)

public_state="$fixture_dir/lib/resgraph/public.state.marshal"
mv "$public_state" "$public_state.bak"
set +e
completion_without_state=$(
  "$repo_dir/bin/dev/resgraph.exe" completion \
    "$fixture_dir/src/public/Public.res" 3 3 \
    "$fixture_dir/src/public/Public.res" public
)
completion_without_state_status=$?
set -e
[[ $completion_without_state_status -eq 0 ]]
[[ "$completion_without_state" == *'"status": "Completion"'* ]]
[[ "$completion_without_state" == *'"label":"gql.field"'* ]]

printf 'invalid state' >"$public_state"
set +e
completion_with_invalid_state=$(
  "$repo_dir/bin/dev/resgraph.exe" completion \
    "$fixture_dir/src/public/Public.res" 3 3 \
    "$fixture_dir/src/public/Public.res" public
)
completion_with_invalid_state_status=$?
set -e
mv "$public_state.bak" "$public_state"
[[ $completion_with_invalid_state_status -eq 0 ]]
[[ "$completion_with_invalid_state" == *'"status": "Completion"'* ]]
[[ "$completion_with_invalid_state" == *'"label":"gql.field"'* ]]

public_definition=$(cd "$fixture_dir" && node "$cli" tools find-definition Query.publicValue --schema public --json)
admin_definition=$(cd "$fixture_dir" && node "$cli" tools find-definition Query.adminValue --schema admin --json)
default_definition=$(cd "$fixture_dir" && node "$cli" tools find-definition Query.publicValue --json)
alternate_order_definition=$(cd "$fixture_dir" && node "$cli" tools find-definition Query.publicValue --json --schema public)
[[ "$public_definition" == *'/src/public/Public.res'* ]]
[[ "$admin_definition" == *'/src/admin/Admin.res'* ]]
[[ "$default_definition" == *'/src/public/Public.res'* ]]
[[ "$alternate_order_definition" == *'/src/public/Public.res'* ]]

set +e
unknown_output=$(cd "$fixture_dir" && node "$cli" build missing 2>&1)
unknown_status=$?
set -e
[[ $unknown_status -ne 0 ]]
[[ "$unknown_output" == *'Unknown ResGraph schema "missing".'* ]]

admin_hash_before=$(sha256sum "$fixture_dir/src/generated/admin/AdminSchema.res" | cut -d ' ' -f 1)
(cd "$fixture_dir" && node "$cli" build public)
admin_hash_after=$(sha256sum "$fixture_dir/src/generated/admin/AdminSchema.res" | cut -d ' ' -f 1)
[[ "$admin_hash_before" == "$admin_hash_after" ]]

capture_watch_output "$fixture_dir" 'Build succeeded' public
[[ "$watch_output" == *'[public] Build succeeded'* ]]
node "$script_dir/multi-schema-lsp.cjs"
node "$script_dir/multi-schema-utils.cjs"

public_generated_backup=$(mktemp -d /tmp/resgraph-public-generated.XXXXXX)
cp "$fixture_dir/src/generated/public/PublicSchema.res" "$public_generated_backup/PublicSchema.res"
cp "$fixture_dir/src/generated/public/PublicSchema.resi" "$public_generated_backup/PublicSchema.resi"
cp "$fixture_dir/src/generated/public/PublicSchema__Interface_entity.res" \
  "$public_generated_backup/PublicSchema__Interface_entity.res"
restore_public_generated() {
  rm -f "$fixture_dir/src/generated/public/RenamedPublicSchema.res"
  rm -f "$fixture_dir/src/generated/public/RenamedPublicSchema.resi"
  rm -f "$fixture_dir/src/generated/public/RenamedPublicSchema__Interface_entity.res"
  cp "$public_generated_backup/PublicSchema.res" "$fixture_dir/src/generated/public/PublicSchema.res"
  cp "$public_generated_backup/PublicSchema.resi" "$fixture_dir/src/generated/public/PublicSchema.resi"
  cp "$public_generated_backup/PublicSchema__Interface_entity.res" \
    "$fixture_dir/src/generated/public/PublicSchema__Interface_entity.res"
  rm -rf "$public_generated_backup"
}
trap restore_public_generated EXIT
rm -rf "$fixture_dir/lib/resgraph"
(cd "$fixture_dir/rename-public" && node "$cli" build public)
test -f "$fixture_dir/src/generated/public/RenamedPublicSchema.res"
test -f "$fixture_dir/src/generated/public/RenamedPublicSchema.resi"
test -f "$fixture_dir/src/generated/public/RenamedPublicSchema__Interface_entity.res"
test ! -e "$fixture_dir/src/generated/public/PublicSchema.res"
test ! -e "$fixture_dir/src/generated/public/PublicSchema.resi"
test ! -e "$fixture_dir/src/generated/public/PublicSchema__Interface_entity.res"
restore_public_generated
trap - EXIT
test -f "$fixture_dir/src/generated/public/PublicSchema.res"
test ! -e "$fixture_dir/src/generated/public/RenamedPublicSchema.res"

set +e
failure_output=$(cd "$fixture_dir/failure" && node "$cli" build 2>&1)
failure_status=$?
set -e
[[ $failure_status -ne 0 ]]
[[ "$failure_output" == *'[uncompiled] Generator process failed.'* ]]
[[ "$failure_output" == *'[broken] Schema generation failed.'* ]]
[[ "$failure_output" == *'[admin] Build succeeded'* ]]
rm -f "$fixture_dir/src/generated/broken/BrokenSchema.res" "$fixture_dir/src/generated/broken/BrokenSchema.resi"

(cd "$fixture_dir/uncompiled" && "$rescript_bin")
for cmt in \
  "$fixture_dir/uncompiled/lib/bs/src/Schema.cmt" \
  "$fixture_dir/uncompiled/lib/ocaml/Schema.cmt"; do
  if [[ -f "$cmt" ]]; then
    printf 'x' >"$cmt"
  fi
done
capture_watch_output "$fixture_dir/failure" '[uncompiled] Generator process failed.' uncompiled
[[ "$watch_output" != *'Build succeeded'* ]]
rm -rf "$fixture_dir/uncompiled/lib"

set +e
empty_output=$(cd "$fixture_dir/empty" && node "$cli" build 2>&1)
empty_status=$?
set -e
[[ $empty_status -ne 0 ]]
[[ "$empty_output" == *'You must define at least a `query` type in your schema.'* ]]
[[ "$empty_output" != *'Generator process failed.'* ]]
rm -f "$fixture_dir/src/generated/empty/EmptySchema.res" "$fixture_dir/src/generated/empty/EmptySchema.resi"

set +e
validation_output=$(cd "$fixture_dir/invalid-config" && node "$cli" build 2>&1)
validation_status=$?
set -e
[[ $validation_status -ne 0 ]]
[[ "$validation_output" == *'use the same outputFolder'* ]]
[[ "$validation_output" == *'use the same authorization manifest or baseline path'* ]]
[[ "$validation_output" == *'use moduleName "CollidingSchema" in the same ReScript package'* ]]

set +e
invalid_exclude_output=$(cd "$fixture_dir/invalid-exclude" && node "$cli" build 2>&1)
invalid_exclude_status=$?
set -e
[[ $invalid_exclude_status -ne 0 ]]
[[ "$invalid_exclude_output" == *'exclude path'*'does not exist'* ]]

membership_alias="$fixture_dir/src/public-alias"
ln -s public "$membership_alias"
membership_config_backup=$(mktemp /tmp/resgraph-membership-config.XXXXXX)
cp "$fixture_dir/resgraph.json" "$membership_config_backup"
restore_membership_config() {
  cp "$membership_config_backup" "$fixture_dir/resgraph.json"
  rm -f "$membership_config_backup" "$membership_alias"
}
trap restore_membership_config EXIT
cp "$fixture_dir/resgraph.symlink-membership.json" "$fixture_dir/resgraph.json"
(cd "$fixture_dir" && node "$cli" build public >/dev/null)
assert_contains "$public_sdl" 'publicValue: String!'
assert_not_contains "$public_sdl" 'experimentalValue'
restore_membership_config
trap - EXIT
(cd "$fixture_dir" && node "$cli" build public >/dev/null)

set +e
case_collision_output=$(cd "$fixture_dir/case-collision" && node "$cli" build 2>&1)
case_collision_status=$?
set -e
[[ $case_collision_status -ne 0 ]]
[[ "$case_collision_output" == *'collide on case-insensitive filesystems'* ]]

output_alias="$fixture_dir/src/generated/public-alias"
ln -s public "$output_alias"
trap 'rm -f "$output_alias"' EXIT
set +e
output_alias_output=$(cd "$fixture_dir/output-alias" && node "$cli" build 2>&1)
output_alias_status=$?
set -e
rm -f "$output_alias"
trap - EXIT
[[ $output_alias_status -ne 0 ]]
[[ "$output_alias_output" == *'use the same outputFolder'* ]]
[[ "$output_alias_output" == *'use the same authorization manifest or baseline path'* ]]

project_alias="$fixture_dir/project-alias"
ln -s . "$project_alias"
trap 'rm -f "$project_alias"' EXIT
project_alias_output=$(cd "$fixture_dir/project-root-alias" && node "$cli" init)
rm -f "$project_alias"
trap - EXIT
[[ "$project_alias_output" == *'Project already set up correctly.'* ]]

(cd "$fixture_dir/package-a" && "$rescript_bin")
legacy_output=$(cd "$fixture_dir/package-a" && node "$cli" build)
[[ "$legacy_output" == 'Build succeeded in '* ]]
test -f "$fixture_dir/package-a/src/generated/ResGraphSchema.res"
test -f "$fixture_dir/package-a/lib/.resgraphState.marshal"
assert_contains "$fixture_dir/package-a/src/generated/ResGraphSchema.resi" 'ResGraph.schema<ResGraphContext.context>'
legacy_state="$fixture_dir/package-a/lib/.resgraphState.marshal"
legacy_state_backup=$(mktemp /tmp/resgraph-legacy-state.XXXXXX)
cp "$legacy_state" "$legacy_state_backup"
restore_legacy_state() {
  cp "$legacy_state_backup" "$legacy_state"
  rm -f "$legacy_state_backup"
}
trap restore_legacy_state EXIT
# OCaml Marshal payload with the old `(schemaState, processedSchema)` shape.
node -e "require('fs').writeFileSync(process.argv[1], Buffer.from(process.argv[2], 'hex'))" \
  "$legacy_state" \
  '8495a6be00000013000000020000001000000010a0080000330040404040404040404040404040'
set +e
old_state_definition=$(cd "$fixture_dir/package-a" && \
  node "$cli" tools find-definition Query --json)
old_state_definition_status=$?
set -e
[[ $old_state_definition_status -ne 0 ]]
[[ "$old_state_definition" == *'could not read the generated schema state'* ]]
[[ "$old_state_definition" == *'Run `resgraph build` again.'* ]]
restore_legacy_state
trap - EXIT
legacy_definition=$(cd "$fixture_dir/package-a" && \
  node "$cli" tools find-definition Query --json)
[[ "$legacy_definition" == *'"path":"Query"'* ]]
[[ "$legacy_definition" == *'"kind":"objectType"'* ]]
capture_watch_output "$fixture_dir/package-a" 'Build succeeded'
[[ "$watch_output" == *'Build succeeded in '* ]]
(cd "$fixture_dir/package-b" && "$rescript_bin")
legacy_generated_backup=$(mktemp -d /tmp/resgraph-legacy-generated.XXXXXX)
cp "$fixture_dir/package-a/src/generated/ResGraphSchema.res" \
  "$legacy_generated_backup/ResGraphSchema.res"
cp "$fixture_dir/package-a/src/generated/ResGraphSchema.resi" \
  "$legacy_generated_backup/ResGraphSchema.resi"
restore_legacy_generated() {
  cp "$legacy_generated_backup/ResGraphSchema.res" \
    "$fixture_dir/package-a/src/generated/ResGraphSchema.res"
  cp "$legacy_generated_backup/ResGraphSchema.resi" \
    "$fixture_dir/package-a/src/generated/ResGraphSchema.resi"
  rm -f "$fixture_dir/package-a/src/generated/interface_obsolete.res"
  rm -f "$fixture_dir/package-a/src/generated/interface_custom.res"
  rm -rf "$legacy_generated_backup"
}
trap restore_legacy_generated EXIT
printf '/* @generated */\n' >"$fixture_dir/package-a/src/generated/interface_obsolete.res"
printf 'let preserved = true\n' >"$fixture_dir/package-a/src/generated/interface_custom.res"
rm -f "$fixture_dir/package-a/lib/resgraph/package-a.incremental-cache"
(cd "$fixture_dir/central" && node "$cli" build)
test ! -e "$fixture_dir/package-a/src/generated/ResGraphSchema.res"
test ! -e "$fixture_dir/package-a/src/generated/ResGraphSchema.resi"
test ! -e "$fixture_dir/package-a/src/generated/interface_obsolete.res"
test -f "$fixture_dir/package-a/src/generated/interface_custom.res"
restore_legacy_generated
trap - EXIT
(cd "$fixture_dir/package-a" && "$rescript_bin")
(cd "$fixture_dir/package-b" && "$rescript_bin")
assert_contains "$fixture_dir/package-a/src/generated/schema.graphql" 'valueA: String!'
assert_contains "$fixture_dir/package-b/src/generated/schema.graphql" 'valueB: String!'
test -f "$fixture_dir/package-a/lib/resgraph/package-a.state.marshal"
test -f "$fixture_dir/package-b/lib/resgraph/packageB.state.marshal"

git -C "$repo_dir" diff --exit-code -- tests/multi-schema/src/generated tests/multi-schema/package-a/src/generated tests/multi-schema/package-b/src/generated

printf 'multi-schema regressions passed\n'
