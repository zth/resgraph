#!/usr/bin/env bash

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

diff=$(git ls-files --modified tests/src/__generated__)
if [[ $diff = "" ]]; then
  printf '%b%s%b\n' "$successGreen" '✅ No unstaged tests difference.' "$reset"
else
  printf '%b%s\n%s\n%b\n' "$warningYellow" \
    '⚠️ There are unstaged differences in generated test outputs!' "$diff" "$reset"
  git --no-pager diff -- tests/src/__generated__
  exit 1
fi

# Compiling generated output can rewrite compiled inputs. Refresh once after
# compilation, then verify that the conservative cache has converged.
../bin/dev/resgraph.exe generate-schema ./src ./src/__generated__ true >/dev/null
incrementalOutput=$(
  RESGRAPH_INCREMENTAL_DEBUG=1 ../bin/dev/resgraph.exe generate-schema \
    ./src ./src/__generated__ true 2>&1
)
if [[ $incrementalOutput != *"Incremental cache hit"* ]]; then
  printf '%b%s\n%s\n%b\n' "$warningYellow" \
    '⚠️ Expected an incremental cache hit.' "$incrementalOutput" "$reset"
  exit 1
fi
printf '%b%s%b\n' "$successGreen" '✅ Incremental schema cache hit.' "$reset"

runtimeChangeOutput=$(
  RESCRIPT_RUNTIME=./node_modules/@rescript/runtime \
    RESGRAPH_INCREMENTAL_DEBUG=1 ../bin/dev/resgraph.exe generate-schema \
    ./src ./src/__generated__ true 2>&1
)
if [[ $runtimeChangeOutput != *"ReScript runtime selection changed"* ]]; then
  printf '%b%s\n%s\n%b\n' "$warningYellow" \
    '⚠️ ReScript runtime change did not invalidate incremental cache.' \
    "$runtimeChangeOutput" "$reset"
  exit 1
fi
../bin/dev/resgraph.exe generate-schema ./src ./src/__generated__ true >/dev/null
printf '%b%s%b\n' "$successGreen" \
  '✅ ReScript runtime changes invalidate incremental cache.' "$reset"

projectCacheSettingOutput=$(
  RESCRIPT_PROJECT_CONFIG_CACHE=true RESGRAPH_INCREMENTAL_DEBUG=1 \
    ../bin/dev/resgraph.exe generate-schema ./src ./src/__generated__ true 2>&1
)
if [[ $projectCacheSettingOutput != *"project config cache setting changed"* ]]; then
  printf '%b%s\n%s\n%b\n' "$warningYellow" \
    '⚠️ Project config cache setting did not invalidate incremental cache.' \
    "$projectCacheSettingOutput" "$reset"
  exit 1
fi
../bin/dev/resgraph.exe generate-schema ./src ./src/__generated__ true >/dev/null
printf '%b%s%b\n' "$successGreen" \
  '✅ Project config cache setting changes invalidate incremental cache.' "$reset"

rescriptVersionOutput=$(
  RESCRIPT_VERSION=99.0 RESGRAPH_INCREMENTAL_DEBUG=1 \
    ../bin/dev/resgraph.exe generate-schema ./src ./src/__generated__ true 2>&1
)
if [[ $rescriptVersionOutput != *"ReScript version selection changed"* ]]; then
  printf '%b%s\n%s\n%b\n' "$warningYellow" \
    '⚠️ ReScript version setting did not invalidate incremental cache.' \
    "$rescriptVersionOutput" "$reset"
  exit 1
fi
../bin/dev/resgraph.exe generate-schema ./src ./src/__generated__ true >/dev/null
printf '%b%s%b\n' "$successGreen" \
  '✅ ReScript version changes invalidate incremental cache.' "$reset"

alternateExecutable=$(mktemp)
cp ../bin/dev/resgraph.exe "$alternateExecutable"
chmod +x "$alternateExecutable"
executableChangeOutput=$(
  RESGRAPH_INCREMENTAL_DEBUG=1 "$alternateExecutable" generate-schema \
    ./src ./src/__generated__ true 2>&1
)
rm -f "$alternateExecutable"
if [[ $executableChangeOutput != *"ResGraph executable changed"* ]]; then
  printf '%b%s\n%s\n%b\n' "$warningYellow" \
    '⚠️ Executable change did not invalidate incremental cache.' \
    "$executableChangeOutput" "$reset"
  exit 1
fi
../bin/dev/resgraph.exe generate-schema ./src ./src/__generated__ true >/dev/null
printf '%b%s%b\n' "$successGreen" \
  '✅ Executable changes invalidate incremental cache.' "$reset"

configBackup=$(mktemp)
symlinkTargets=$(mktemp -d)
cp ./rescript.json "$configBackup"
mkdir "$symlinkTargets/a" "$symlinkTargets/b"
ln -s "$symlinkTargets/a" ./cache-link-src
node -e '
  const fs = require("fs")
  const config = JSON.parse(fs.readFileSync("rescript.json", "utf8"))
  config.sources.push("cache-link-src")
  fs.writeFileSync("rescript.json", JSON.stringify(config, null, 2) + "\n")
'
../bin/dev/resgraph.exe generate-schema ./src ./src/__generated__ true >/dev/null
rm ./cache-link-src
ln -s "$symlinkTargets/b" ./cache-link-src
symlinkChangeOutput=$(
  RESGRAPH_INCREMENTAL_DEBUG=1 ../bin/dev/resgraph.exe generate-schema \
    ./src ./src/__generated__ true 2>&1
)
cp "$configBackup" ./rescript.json
rm -f "$configBackup" ./cache-link-src
rm -r "$symlinkTargets"
if [[ $symlinkChangeOutput != *"project input changed"* ]]; then
  printf '%b%s\n%s\n%b\n' "$warningYellow" \
    '⚠️ Symlink retarget did not invalidate incremental cache.' \
    "$symlinkChangeOutput" "$reset"
  exit 1
fi
../bin/dev/resgraph.exe generate-schema ./src ./src/__generated__ true >/dev/null
printf '%b%s%b\n' "$successGreen" \
  '✅ Symlink retargets invalidate incremental cache.' "$reset"

printf '{}\n' >./bsconfig.json
configAdditionOutput=$(
  RESGRAPH_INCREMENTAL_DEBUG=1 ../bin/dev/resgraph.exe generate-schema \
    ./src ./src/__generated__ true 2>&1
)
rm -f ./bsconfig.json
if [[ $configAdditionOutput != *"project input changed"* ]]; then
  printf '%b%s\n%s\n%b\n' "$warningYellow" \
    '⚠️ Added configuration file did not invalidate incremental cache.' \
    "$configAdditionOutput" "$reset"
  exit 1
fi
../bin/dev/resgraph.exe generate-schema ./src ./src/__generated__ true >/dev/null
printf '%b%s%b\n' "$successGreen" \
  '✅ Added configuration files invalidate incremental cache.' "$reset"

dependencyConfig=./node_modules/@rescript/react/rescript.json
dependencyConfigBackup=$(mktemp)
cp "$dependencyConfig" "$dependencyConfigBackup"
printf '\n' >>"$dependencyConfig"
dependencyConfigOutput=$(
  RESGRAPH_INCREMENTAL_DEBUG=1 ../bin/dev/resgraph.exe generate-schema \
    ./src ./src/__generated__ true 2>&1
)
cp "$dependencyConfigBackup" "$dependencyConfig"
rm -f "$dependencyConfigBackup"
if [[ $dependencyConfigOutput != *"project input changed"* ]]; then
  printf '%b%s\n%s\n%b\n' "$warningYellow" \
    '⚠️ Dependency configuration change did not invalidate cache.' \
    "$dependencyConfigOutput" "$reset"
  exit 1
fi
../bin/dev/resgraph.exe generate-schema ./src ./src/__generated__ true >/dev/null
printf '%b%s%b\n' "$successGreen" \
  '✅ Dependency configuration changes invalidate incremental cache.' "$reset"

hiddenDependency=./node_modules/resgraph-hidden-dependency
hiddenConfigBackup=$(mktemp)
cp ./rescript.json "$hiddenConfigBackup"
mkdir -p "$hiddenDependency/src" "$hiddenDependency/lib"
printf '%s\n' \
  '{"name":"resgraph-hidden-dependency","sources":["src"],"public":[]}' \
  >"$hiddenDependency/rescript.json"
node -e '
  const fs = require("fs")
  const config = JSON.parse(fs.readFileSync("rescript.json", "utf8"))
  config.dependencies.push("resgraph-hidden-dependency")
  fs.writeFileSync("rescript.json", JSON.stringify(config, null, 2) + "\n")
'
../bin/dev/resgraph.exe generate-schema ./src ./src/__generated__ true >/dev/null
mkdir "$hiddenDependency/lib/bs"
printf 'not a real cmt' >"$hiddenDependency/lib/bs/FirstModule.cmt"
hiddenDependencyOutput=$(
  RESGRAPH_INCREMENTAL_DEBUG=1 ../bin/dev/resgraph.exe generate-schema \
    ./src ./src/__generated__ true 2>&1
)
cp "$hiddenConfigBackup" ./rescript.json
rm -f "$hiddenConfigBackup" "$hiddenDependency/rescript.json" \
  "$hiddenDependency/lib/bs/FirstModule.cmt"
rmdir "$hiddenDependency/src" "$hiddenDependency/lib/bs" \
  "$hiddenDependency/lib" "$hiddenDependency"
if [[ $hiddenDependencyOutput != *"project input changed"* ]]; then
  printf '%b%s\n%s\n%b\n' "$warningYellow" \
    '⚠️ First compiled dependency module did not invalidate cache.' \
    "$hiddenDependencyOutput" "$reset"
  exit 1
fi
../bin/dev/resgraph.exe generate-schema ./src ./src/__generated__ true >/dev/null
printf '%b%s%b\n' "$successGreen" \
  '✅ Dependencies without visible modules remain tracked.' "$reset"

compiledConfigBackup=$(mktemp)
cp ./rescript.json "$compiledConfigBackup"
mkdir ./empty-compiled-src ./lib/bs/empty-compiled-src
node -e '
  const fs = require("fs")
  const config = JSON.parse(fs.readFileSync("rescript.json", "utf8"))
  config.sources.push("empty-compiled-src")
  fs.writeFileSync("rescript.json", JSON.stringify(config, null, 2) + "\n")
'
../bin/dev/resgraph.exe generate-schema ./src ./src/__generated__ true >/dev/null
printf 'not a real cmt' >./lib/bs/empty-compiled-src/NewModule.cmt
compiledDirectoryOutput=$(
  RESGRAPH_INCREMENTAL_DEBUG=1 ../bin/dev/resgraph.exe generate-schema \
    ./src ./src/__generated__ true 2>&1
)
cp "$compiledConfigBackup" ./rescript.json
rm -f "$compiledConfigBackup" ./lib/bs/empty-compiled-src/NewModule.cmt
rmdir ./empty-compiled-src ./lib/bs/empty-compiled-src
if [[ $compiledDirectoryOutput != *"project input changed"* ]]; then
  printf '%b%s\n%s\n%b\n' "$warningYellow" \
    '⚠️ New compiled module did not invalidate incremental cache.' \
    "$compiledDirectoryOutput" "$reset"
  exit 1
fi
../bin/dev/resgraph.exe generate-schema ./src ./src/__generated__ true >/dev/null
printf '%b%s%b\n' "$successGreen" \
  '✅ Compiled directories without discovered modules remain tracked.' "$reset"

sourceBackup=$(mktemp)
cp ./src/ResGraphContext.res "$sourceBackup"
printf '\n// Conservative cache input probe.\n' >>./src/ResGraphContext.res
inputChangeOutput=$(
  RESGRAPH_INCREMENTAL_DEBUG=1 ../bin/dev/resgraph.exe generate-schema \
    ./src ./src/__generated__ true 2>&1
)
cp "$sourceBackup" ./src/ResGraphContext.res
rm -f "$sourceBackup"
if [[ $inputChangeOutput != *"project input changed"* ]]; then
  printf '%b%s\n%s\n%b\n' "$warningYellow" \
    '⚠️ Source change did not invalidate incremental cache.' \
    "$inputChangeOutput" "$reset"
  exit 1
fi
../bin/dev/resgraph.exe generate-schema ./src ./src/__generated__ true >/dev/null
printf '%b%s%b\n' "$successGreen" \
  '✅ Source changes invalidate incremental cache.' "$reset"

schemaBackup=$(mktemp)
cp ./src/__generated__/ResGraphSchema.res "$schemaBackup"
printf '\n// Cache integrity probe.\n' >>./src/__generated__/ResGraphSchema.res
tamperOutput=$(
  RESGRAPH_INCREMENTAL_DEBUG=1 ../bin/dev/resgraph.exe generate-schema \
    ./src ./src/__generated__ true 2>&1
)
if [[ $tamperOutput != *"Incremental cache miss"* ]] || \
  ! cmp -s "$schemaBackup" ./src/__generated__/ResGraphSchema.res; then
  printf '%b%s\n%s\n%b\n' "$warningYellow" \
    '⚠️ Incremental cache did not repair changed output.' "$tamperOutput" "$reset"
  cp "$schemaBackup" ./src/__generated__/ResGraphSchema.res
  rm -f "$schemaBackup"
  exit 1
fi
rm -f "$schemaBackup"
printf '%b%s%b\n' "$successGreen" \
  '✅ Incremental cache repairs changed output.' "$reset"

printf 'invalid cache' >./lib/.resgraphIncrementalCache
invalidCacheOutput=$(
  RESGRAPH_INCREMENTAL_DEBUG=1 ../bin/dev/resgraph.exe generate-schema \
    ./src ./src/__generated__ true 2>&1
)
if [[ $invalidCacheOutput != *"could not be read"* ]]; then
  printf '%b%s\n%s\n%b\n' "$warningYellow" \
    '⚠️ Invalid incremental cache was not rejected.' "$invalidCacheOutput" "$reset"
  exit 1
fi
printf '%b%s%b\n' "$successGreen" \
  '✅ Invalid incremental cache is rebuilt safely.' "$reset"

node ./runtime-interface-returns.mjs
node ./runtime-compat.mjs
node ./runtime-directives.mjs
bash ./schema-marker.sh
node ./runtime-oneof.mjs
bash ./invalid-directives.sh
./invalid-interface-implements.sh

./multi-schema-test.sh
