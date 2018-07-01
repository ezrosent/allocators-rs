#!/bin/bash

# Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
# README.md file at the top-level directory of this repository.
#
# Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
# the MIT license (the LICENSE-MIT file) at your option. This file may not be
# copied, modified, or distributed except according to those terms.

# modification_years <file>
function modification_years {
  # TODO(joshlf): I've determined that the 'git log' command is taking up the
  # majority of the execution time of this script. It's unclear whether there's
  # much opportunity for optimizing it, but it would be the primary target for
  # it.
  FIRST_LAST_YEARS=$(git log --pretty=format:"%ci" "$1" | awk 'NR==1; END{print}' | cut -d - -f 1 | tr '\n' ' ')
  FIRST_YEAR=$(echo "$FIRST_LAST_YEARS" | cut -d ' ' -f 2)
  LAST_YEAR=$(echo "$FIRST_LAST_YEARS" | cut -d ' ' -f 1)
  if [ "$FIRST_YEAR" == "$LAST_YEAR" ]; then
    echo "$FIRST_YEAR"
  else
    echo "${FIRST_YEAR}-${LAST_YEAR}"
  fi
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

  for file in $(git ls-tree -r HEAD --name-only | grep "${1}$"); do
    # We're either looking for a comment of the form 'Copyright 2017'
    # or of the form 'Copyright 2017-2018'.
    YEARS=$(modification_years "$file")
    # Including " the authors." is important, or else "Copyright 2017-2018 the authors."
    # would be spuriously matched if the correct $YEARS was 2017.
    COMMENT="$2 Copyright $YEARS the authors."

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
