#!/usr/bin/env python3
"""講義資料の引用を, repo の bib と論文 DB の検証の水準に突き合わせる.

使い方:
    uv run --no-project python scripts/check_citations.py [--papers DIR] [--no-write]

対象は front matter に ``bibliography: <name>`` を書いた ``lectures/**/*.md``.
本文の引用キー ([@key], @key) ごとに次を調べる.

失敗 (exit 1, publish を止める):
  - キーが ``bib/<name>.bib`` に無い.
  - 論文 DB に note が無いか, 検証の水準が none (書誌の実在を確かめていない).
  - ``bibliography`` で指定した bib が無い.
警告 (publish は止めない):
  - 検証の水準が identity か abstract (本文を確かめていない = 未照合の引用).
  - 引用があるのに文献表の位置 (``::: {#refs}``) が無い.
  - ``bibliography`` の指定が無い章に ``[@key]`` がある (処理されず元の文字列が出る).

未照合の引用の一覧は ``writing/citations/unverified.md`` に書き出す (``--no-write`` で抑止).
講義資料は本文を確かめる前でも引用して公開してよい (2026-11-20 本人裁定). 書誌の実在
(identity) だけは公開前に確かめる. 計画は plan/in-progress/lecture-literature-grounding.md.
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
DEFAULT_PAPERS = Path.home() / "Documents" / "Papers"
LEVELS = ["none", "identity", "abstract", "partial", "full"]
VERIFIED = {"partial", "full"}

FRONT_MATTER = re.compile(r"\A---\n(.*?)\n---\n", re.S)
FENCE = re.compile(r"^(```|~~~)")
INLINE_CODE = re.compile(r"`[^`]*`")
# pandoc の引用キー: @ の直前が語の文字でない. キーは英数字か _ で始まる.
CITE = re.compile(r"(?<![\w@.])@([A-Za-z0-9_][\w:.#$%&+?<>~/-]*[\w])")
BRACKET_CITE = re.compile(r"\[[^\]]*?@[A-Za-z0-9_][^\]]*\]")
REFS_DIV = re.compile(r"^:::+\s*\{[^}]*#refs\b", re.M)
BIB_KEY = re.compile(r"^@\w+\s*\{\s*([^,\s]+)\s*,", re.M)
LEVEL_LINE = re.compile(r"^verification:\s*\n\s+level:\s*([a-z]+)", re.M)


def body_lines(text: str):
    """コードブロックとインラインコードを除いた本文の行を (行番号, 行) で返す."""
    in_fence = False
    for no, line in enumerate(text.splitlines(), 1):
        if FENCE.match(line.lstrip()):
            in_fence = not in_fence
            continue
        if in_fence:
            continue
        yield no, INLINE_CODE.sub("", line)


def bib_keys(path: Path) -> set[str]:
    return set(BIB_KEY.findall(path.read_text(encoding="utf-8")))


def papers_level(papers: Path, key: str) -> str | None:
    note = papers / "_db" / "notes" / f"{key}.md"
    if not note.exists():
        return None
    m = LEVEL_LINE.search(note.read_text(encoding="utf-8"))
    return m.group(1) if m else "none"


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--papers", type=Path, default=DEFAULT_PAPERS, help="論文 DB の root")
    ap.add_argument("--no-write", action="store_true", help="未照合の一覧を書き出さない")
    args = ap.parse_args()

    if not (args.papers / "_db" / "notes").is_dir():
        print(f"論文 DB が見つかりません: {args.papers}", file=sys.stderr)
        return 2

    failures: list[str] = []
    warnings: list[str] = []
    unverified: list[tuple[str, str, str]] = []  # (章, キー, 水準)
    n_pages = n_cites = 0

    for md in sorted((REPO / "lectures").rglob("*.md")):
        rel = md.relative_to(REPO)
        text = md.read_text(encoding="utf-8")
        fm = FRONT_MATTER.match(text)
        meta = fm.group(1) if fm else ""
        body = text[fm.end():] if fm else text
        m = re.search(r"^bibliography:\s*(\S+)\s*$", meta, re.M)

        if not m:
            for no, line in body_lines(body):
                if BRACKET_CITE.search(line):
                    warnings.append(f"{rel}:{no}: bibliography の指定が無い章に引用があります: {line.strip()[:80]}")
            continue

        n_pages += 1
        bib = REPO / "bib" / f"{m.group(1)}.bib"
        if not bib.exists():
            failures.append(f"{rel}: 指定の bib がありません: {bib.relative_to(REPO)}")
            continue
        keys_in_bib = bib_keys(bib)

        seen: dict[str, int] = {}
        for no, line in body_lines(body):
            for key in CITE.findall(line):
                seen.setdefault(key, no)
        if seen and not REFS_DIV.search(body):
            warnings.append(f"{rel}: 文献表の位置 (::: {{#refs}}) がありません. 文献表は末尾に入ります")

        for key, no in seen.items():
            n_cites += 1
            if key not in keys_in_bib:
                failures.append(f"{rel}:{no}: @{key} が {bib.relative_to(REPO)} にありません")
                continue
            level = papers_level(args.papers, key)
            if level is None:
                failures.append(f"{rel}:{no}: @{key} の note が論文 DB にありません")
            elif level == "none":
                failures.append(f"{rel}:{no}: @{key} は書誌の実在を確かめていません (verification: none)")
            elif level not in VERIFIED:
                unverified.append((str(rel), key, level))

    for w in warnings:
        print(f"警告: {w}")
    for k in unverified:
        print(f"未照合: {k[0]}: @{k[1]} (verification: {k[2]})")
    for f in failures:
        print(f"失敗: {f}")
    print(f"文献を指定した章 {n_pages} 件, 引用キー {n_cites} 件 (章ごとの延べ), "
          f"未照合 {len(unverified)} 件, 警告 {len(warnings)} 件, 失敗 {len(failures)} 件")

    if not args.no_write:
        out = REPO / "writing" / "citations" / "unverified.md"
        out.parent.mkdir(parents=True, exist_ok=True)
        lines = [
            "# 未照合の引用",
            "",
            "`scripts/check_citations.py` が書き出す一覧 (手で編集しない). 本文を確かめていない引用で,",
            "照合して論文 DB の verification を partial 以上にすると一覧から消える.",
            "",
            "| 章 | キー | verification |",
            "|---|---|---|",
        ]
        lines += [f"| {c} | {k} | {lv} |" for c, k, lv in unverified]
        if not unverified:
            lines.append("| (なし) | | |")
        out.write_text("\n".join(lines) + "\n", encoding="utf-8")

    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
