#!/usr/bin/env python3
"""変更した講義資料のページに, 補足と注意の囲み (note, warn) が 1 つも無ければ警告する.

使い方:
    uv run --no-project python scripts/check_callouts.py [--base REF] [FILE ...]

対象は ``lectures/**/*.md``, ``fields/**/*.md``, ``courses/**/*.md``. FILE を渡さないときは,
``--base`` (既定 ``origin/main``) から作業ツリーまでに変更したファイルと, git が追跡していない
新しいファイルを調べる. 囲みは ``::: note``, ``::: warn``, ``::: {.note}``, ``::: {.warn}`` の形を数え,
コードブロックの中は数えない.

警告だけで, publish は止めない (exit 0). 囲みが要らないページもあるため, 警告が出たら
新しく書いたり大きく直したりした節に補足と注意が埋もれていないかを読み直す
(writing/local/lecture-discourse.md の 7). 囲みの指摘が 2 回続いたこと (2026-11-23, 2026-11-27) を受けて足した.
"""
from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
TARGET_DIRS = ("lectures/", "fields/", "courses/")
FENCE = re.compile(r"^(```|~~~)")
CALLOUT = re.compile(r"^:::+\s*(?:\{\s*\.)?(note|warn)\b")


def count_callouts(text: str) -> int:
    """コードブロックの外にある note と warn の囲みの開始行を数える."""
    in_fence = False
    count = 0
    for line in text.splitlines():
        stripped = line.lstrip()
        if FENCE.match(stripped):
            in_fence = not in_fence
            continue
        if not in_fence and CALLOUT.match(stripped):
            count += 1
    return count


def is_target(path: str) -> bool:
    return path.endswith(".md") and path.startswith(TARGET_DIRS)


def git_lines(*args: str) -> list[str]:
    out = subprocess.run(
        ["git", *args], cwd=REPO, check=True, capture_output=True, text=True
    ).stdout
    return [line for line in out.splitlines() if line]


def changed_files(base: str) -> list[str]:
    """base から作業ツリーまでに変更したファイルと, 追跡していない新しいファイル."""
    changed = git_lines("diff", "--name-only", "--diff-filter=AMR", base, "--")
    untracked = git_lines("ls-files", "--others", "--exclude-standard")
    return sorted({p for p in changed + untracked if is_target(p)})


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--base", default="origin/main", help="比べる基準 (既定 origin/main)")
    parser.add_argument("files", nargs="*", help="調べるファイル (省略時は変更したファイル)")
    args = parser.parse_args()

    if args.files:
        files = [f for f in args.files if is_target(f)]
    else:
        try:
            files = changed_files(args.base)
        except subprocess.CalledProcessError as err:
            print(f"警告: 変更したファイルを git から取れませんでした ({err.stderr.strip()})", file=sys.stderr)
            return 0

    missing = []
    for rel in files:
        path = REPO / rel
        if path.is_file() and count_callouts(path.read_text(encoding="utf-8")) == 0:
            missing.append(rel)

    for rel in missing:
        print(f"警告: {rel}: 補足と注意の囲み (note, warn) が 1 つもありません. "
              "新しく書いた節に, 囲みに入れる補足や注意が埋もれていないかを確かめてください.")
    print(f"変更した講義資料 {len(files)} 件, 囲みの無いページ {len(missing)} 件")
    return 0


if __name__ == "__main__":
    sys.exit(main())
