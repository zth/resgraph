#!/usr/bin/env bash
###############################################################################
# Post build hook for ReScript output                                         #
#                                                                             #
# Performs clean-up of specific files such as prepending a shebang to the CLI #
###############################################################################

if [ "$1" == "../../cli/Cli.mjs" ]; then
  echo "#!/usr/bin/env node" > "$1.tmp"
  cat "$1" >> "$1.tmp"
  mv "$1.tmp" $1
  chmod +x $1
fi
