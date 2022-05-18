#!/usr/bin/env bash

# This script serves as a sanity check because the test suite doesn't test the actual Docker image.

set -euo pipefail

imageTag="${1:?image tag not specified}"

RED=$'\e[31m'
GREEN=$'\e[32m'
NOCOLOR=$'\e[39m'

checkTestDockerfile() {
    testDockerfile="${1}"
    actualOutputFile="$(mktemp)"
    docker run -i "${imageTag}" < "${testDockerfile}" > "${actualOutputFile}"
    echo >> "${actualOutputFile}" # The actual output should not contain a trailing newline, but the expected-output file does.
    echo "Checking final output (input: ${testDockerfile}) (${RED}-expected${NOCOLOR}, ${GREEN}+actual${NOCOLOR}) …"
    git \
        -c color.diff.old=red \
        -c color.diff.new=green \
        diff \
        --color=always \
        --no-index \
        "${testDockerfile}.out" \
        "${actualOutputFile}" \
        | tail -n +5 # to remove confusing/irrelevant patch header
}

for f in test/testdata/*; do
    if [[ ! "$f" =~ \.out$ ]]; then
        checkTestDockerfile "${f}"
    fi
done
