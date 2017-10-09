#!/bin/bash

# Copyright 2017 the authors. See the 'Copyright and license' section of the
# README.md file at the top-level directory of this repository.
#
# Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
# the MIT license (the LICENSE-MIT file) at your option. This file may not be
# copied, modified, or distributed except according to those terms.

# last_modification_year <file>
function last_modification_year {
  COMMIT=$(git log --pretty=format:"%H" "$1" | head -n 1)
  git show -s --format=%ci "$COMMIT" | cut -d - -f 1
}

# first_modification_year <file>
function first_modification_year {
  COMMIT=$(git log --pretty=format:"%H" "$1" | tail -n 1)
  git show -s --format=%ci "$COMMIT" | cut -d - -f 1
}

# like check_comments, checks the <line>th line for the comment
#
# check_comments_shell <file extension> <comment prefix> <line>
function check_comments_line {
  RET=0
  # NOTE: This will break if any of the files have spaces in them,
  # but the other alternative is to pipe the output of find into
  # 'while read file', which spawns a subshell, and thus prevents
  # us from setting the RET variable from inside the loop.
  for file in $(find . -name "*${1}"); do
    # Don't check files that aren't in version control
    if ! git ls-files --error-unmatch "$file" >/dev/null 2>/dev/null; then
      continue
    fi

    # We're either looking for a comment of the form 'Copyright 2017'
    # or of the form 'Copyright 2017-2018'.
    FIRST_YEAR=$(first_modification_year "$file")
    LAST_YEAR=$(last_modification_year "$file")
    YEAR=$(if [ "$FIRST_YEAR" == "$LAST_YEAR" ]; then
      echo "$FIRST_YEAR"
    else
      echo "${FIRST_YEAR}-${LAST_YEAR}"
    fi)
    COMMENT="$2 Copyright $YEAR"

    head -n "$3" "$file" | tail -n 1 | grep "$COMMENT" >/dev/null
    if [ $? -ne 0 ]; then
      echo "$file: missing or malformed copyright comment" >&2
      RET=1
    fi
  done

  return "$RET"
}

# check_comments <file extension> <comment prefix>
function check_comments {
  check_comments_line "$1" "$2" 1
}

EXIT=0
check_comments '.rs' '//' || EXIT=1
check_comments '.md' '<!--' || EXIT=1
check_comments '.toml' '#' || EXIT=1
check_comments '.yml' '#' || EXIT=1
# In shell scripts, the copyright comment should start on line 3
check_comments_line '.sh' '#' 3 || EXIT=1
exit "$EXIT"
