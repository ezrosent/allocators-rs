#!/bin/bash

# Copyright 2017 the authors. See the 'Copyright and license' section of the
# README.md file at the top-level directory of this repository.
#
# Licensed under the Apache License, Version 2.0 (the LICENSE file). This file
# may not be copied, modified, or distributed except according to those terms.

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
    head -n "$3" "$file" | tail -n 1 | grep "$2" >/dev/null
    if [ $? -ne 0 ]; then
      echo "$file: missing copyright comment" >&2
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
check_comments '.rs' '// Copyright' || EXIT=1
check_comments '.md' '<!-- Copyright' || EXIT=1
check_comments '.toml' '# Copyright' || EXIT=1
# In shell scripts, the copyright comment should start on line 3
check_comments_line '.sh' '# Copyright' 3 || EXIT=1
exit "$EXIT"
