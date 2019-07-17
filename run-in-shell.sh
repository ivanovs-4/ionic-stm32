#!/usr/bin/env sh
shell_file=${1}
shift
set -ex
nix-shell ${shell_file} --run "$*"
