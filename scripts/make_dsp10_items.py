#!/usr/bin/env python3
"""日本標準産業分類の製造業の内容例示から, 品目名と中分類の表を作る (dsp 第10章の配布データ).

入力: 総務省の「日本標準産業分類 (令和5年7月告示) 分類項目名, 説明及び内容例示」の
大分類 E 製造業の PDF (https://www.soumu.go.jp/main_content/000935526.pdf) を
`pdftotext -layout` で文字にした file.
出力: CSV (列 item, mid_code, mid_name, small_code, fine_code). 1 行が 1 品目.

使い方:
    pdftotext -layout 000935526.pdf mfg.txt
    uv run --no-project python scripts/make_dsp10_items.py mfg.txt slds_data/dsp10/manufacturing_items.csv

抜き出しの規則:
- 細分類 (4 桁の番号) ごとに, 「○」(その細分類に含まれる例) と「×」(含まれない例. 角括弧に
  正しい細分類の番号が付く) の並びを読む. × の例は正しい番号に付け替える. 製造業 (中分類
  09〜32) の外に付け替わる例は捨てる.
- 管理, 補助的経済活動を行う事業所 (細分類の番号の 3 桁目が 0) は除く.
- 例の末尾の「製造業」を落として品目名にする (「バター製造業」→「バター」).
- 同じ品目名が複数の中分類に出るものは除く. 同じ中分類の中で重なるものは最初の 1 件を残す.
"""

import csv
import re
import sys
import unicodedata
from pathlib import Path

FINE_LINE = re.compile(r"^(\d{4})\s{2,}\S")
SMALL_LINE = re.compile(r"^\d{3}\s{2,}\S")
MID_HEADING = re.compile(r"^中分類\s*(\d{2})\s*-\s*(.+)$")
CROSS_REF = re.compile(r"^(.*?)\[(\d{4})\]$")
SUFFIX = re.compile(r"製造業(?=\(|$)")


def read_text(path: Path) -> list[str]:
    """PDF から起こした文字を NFKC で正規化し, 行の前後の空白を落として返す."""
    text = unicodedata.normalize("NFKC", path.read_text(encoding="utf-8"))
    return [line.strip() for line in text.split("\n")]


def is_manufacturing(code: str) -> bool:
    return 9 <= int(code[:2]) <= 32


def parse(lines: list[str]) -> tuple[dict[str, str], list[tuple[str, str, str]]]:
    """中分類の名前と, (細分類の番号, 例の文字列, ○ か ×) の並びを返す.

    中分類の見出しは「中分類09-食料品製造業」の形の行だけを読む. 説明文の中の
    「中分類 24[2445] に分類される」は見出しとして扱わない.
    """
    mid_names: dict[str, str] = {}
    blocks: dict[tuple[str, str], list[str]] = {}
    code = None
    mark = None
    for line in lines:
        heading = MID_HEADING.match(line)
        if heading:
            mid_names[heading.group(1)] = re.sub(r"\s+", "", heading.group(2))
            code = None
            continue
        fine = FINE_LINE.match(line)
        if fine:
            code = fine.group(1) if is_manufacturing(fine.group(1)) else None
            mark = None
            continue
        if SMALL_LINE.match(line):
            code = None
            continue
        if code is None or not line:
            continue
        if line.startswith("○"):
            mark, line = "o", line[1:]
        elif line.startswith("×"):
            mark, line = "x", line[1:]
        elif mark is None:
            continue
        blocks.setdefault((code, mark), []).append(line)

    examples = []
    for (code, mark), parts in blocks.items():
        for example in "".join(parts).split(";"):
            example = example.strip()
            if not example:
                continue
            if mark == "o":
                examples.append((code, example, mark))
                continue
            cross = CROSS_REF.match(example)
            if cross:
                examples.append((cross.group(2), cross.group(1).strip(), mark))
    return mid_names, examples


def build_rows(
    mid_names: dict[str, str], examples: list[tuple[str, str, str]]
) -> list[dict[str, str]]:
    """例を品目名にし, 中分類が 1 つに決まるものだけを 1 品目 1 行で返す (○ の例を先に置く)."""
    examples = sorted(examples, key=lambda e: e[2] != "o")
    examples = [
        (code, SUFFIX.sub("", name), mark)
        for code, name, mark in examples
        if is_manufacturing(code) and code[2] != "0"
    ]
    mids_of: dict[str, set[str]] = {}
    for code, item, _ in examples:
        mids_of.setdefault(item, set()).add(code[:2])

    rows = []
    seen = set()
    for code, item, _ in examples:
        if not item or len(mids_of[item]) > 1 or item in seen:
            continue
        seen.add(item)
        rows.append(
            {
                "item": item,
                "mid_code": code[:2],
                "mid_name": mid_names[code[:2]],
                "small_code": code[:3],
                "fine_code": code,
            }
        )
    return sorted(rows, key=lambda r: (r["fine_code"], r["item"]))


def write_rows(rows: list[dict[str, str]], path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


def main() -> None:
    source, target = Path(sys.argv[1]), Path(sys.argv[2])
    mid_names, examples = parse(read_text(source))
    if len(mid_names) != 24:
        sys.exit(f"中分類の見出しが 24 個ではありません: {sorted(mid_names)}")
    rows = build_rows(mid_names, examples)
    write_rows(rows, target)
    print(f"{len(rows)} 品目を {target} に書きました.")


if __name__ == "__main__":
    main()
