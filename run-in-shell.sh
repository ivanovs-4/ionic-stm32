#!/usr/bin/env sh
set -euo pipefail
shell_file=${1}
shift
# set -x
nix-shell ${shell_file} --run "$*"
