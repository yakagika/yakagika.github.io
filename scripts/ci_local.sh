#!/usr/bin/env bash
set -euo pipefail

CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.."

run_step() {
    local name="$1"
    local started=$SECONDS
    local status
    shift

    printf 'START %s\n' "$name"
    if "$@"; then
        printf 'PASS  %s (%ds)\n' "$name" "$((SECONDS - started))"
    else
        status=$?
        printf 'FAIL  %s (%ds, exit %d)\n' "$name" "$((SECONDS - started))" "$status" >&2
        exit "$status"
    fi
}

test_fp_examples() (
    CDPATH= cd fp-examples && stack test
)

run_step 'check_citations.py' uv run --no-project python scripts/check_citations.py
run_step 'stack build' stack build
run_step 'stack exec main rebuild' stack exec main rebuild
run_step 'check_local_refs.py docs' uv run --no-project python scripts/check_local_refs.py docs
run_step 'check_reg_old_anchors.py docs' python3 scripts/check_reg_old_anchors.py docs
run_step 'test_block_site.py' python3 scripts/test_block_site.py
run_step 'test_check_local_refs.py' python3 -m unittest scripts.test_check_local_refs
run_step 'test_reg_gpa.py' uv run --no-project --with numpy --with pandas python scripts/test_reg_gpa.py
run_step 'fp-examples stack test' test_fp_examples
