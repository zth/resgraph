warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

# Parity check: run direct generator to a temp dir and diff with committed outputs.
tmpGenerated=$(mktemp -d)
../bin/dev/resgraph.exe generate-schema-direct ./src "$tmpGenerated" true

if ! diff -ruN --exclude='*.mjs' ./src/__generated__ "$tmpGenerated" > /dev/null; then
  printf "${warningYellow}⚠️  Direct generator output differs from committed test outputs.${reset}\n"
  diff -ruN --exclude='*.mjs' ./src/__generated__ "$tmpGenerated"
  rm -rf "$tmpGenerated"
  exit 1
fi

rm -rf "$tmpGenerated"

diff=$(git ls-files --modified tests/src/__generated__)
if [[ $diff = "" ]]; then
  printf "${successGreen}✅ No unstaged tests difference.${reset}\n"
else
  printf "${warningYellow}⚠️ There are unstaged differences in generated test outputs!\n${diff}\n${reset}"
  git --no-pager diff -- tests/src/__generated__
  exit 1
fi
