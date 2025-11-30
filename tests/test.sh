warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

diff=$(git ls-files --modified tests/src/__generated__)
if [[ $diff = "" ]]; then
  printf "${successGreen}✅ No unstaged tests difference.${reset}\n"
else
  printf "${warningYellow}⚠️ There are unstaged differences in generated test outputs!\n${diff}\n${reset}"
  git --no-pager diff -- tests/src/__generated__
  exit 1
fi
