#!/bin/bash

# Copyright 2018 the authors. See the 'Copyright and license' section of the
# README.md file at the top-level directory of this repository.
#
# Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
# the MIT license (the LICENSE-MIT file) at your option. This file may not be
# copied, modified, or distributed except according to those terms.

echo "Executing pre-commit hooks..."

# source: http://stackoverflow.com/a/957978/836390
ROOT="$(git rev-parse --show-toplevel)" || exit 1

if [ "$ROOT" == "" ]; then
	echo "`git rev-parse --show-toplevel` returned empty root path" >&2
	exit 1
fi

cd "$ROOT"

function die {
	if [ $# -eq 1 ]; then
		rm "$1"
	fi
	echo "commit aborted" >&1
	exit 1
}

echo "  Running ./test-scripts/check-copyright-comments.sh..."
./test-scripts/check-copyright-comments.sh
