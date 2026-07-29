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
./invalid-interface-implements.sh
