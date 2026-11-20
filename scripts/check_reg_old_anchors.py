#!/usr/bin/env python3
"""旧 DSP 第 5 章の ID と回帰分析のブロックへの対応を調べる.

使い方: python3 scripts/check_reg_old_anchors.py [docs]
旧 ID は生成済み HTML で, 行き先はブロックの Markdown で調べる.
成功時は旧 ID と行き先の URL を TSV で出力する.
問題があれば該当する項目を列挙して終了コード 1 を返す.
"""

import re
import sys
from pathlib import Path
from urllib.parse import quote, unquote

from check_local_refs import RefCollector


TSV_HEADER = "old_id\tblock\tblock_id"
BLOCK_NAME = re.compile(r"[a-z0-9-]+")
DSP5_LINK = re.compile(r"""(?<![\w.-])dsp5\.html#([^\s<>()"'`\]]+)""")
FENCE = re.compile(r"^ {0,3}(`{3,}|~{3,})(.*)$")
HEADING = re.compile(r"^(#{1,6})[ \t]+(.*)$")
HEADING_ID = re.compile(r"[ \t]+\{#([^\s}]+)(?:[ \t]+[^}]*)?\}[ \t]*$")


def read_mappings(path: Path, issues: list[str]) -> dict[str, tuple[str, str]]:
    """TSV の行を旧 ID で引けるようにする."""
    with path.open(encoding="utf-8") as source:
        if source.readline().rstrip("\r\n") != TSV_HEADER:
            issues.append(f"{path.name}: 列見出しは {TSV_HEADER} です")
        mappings = {}
        for number, line in enumerate(source, start=2):
            columns = line.rstrip("\r\n").split("\t")
            if len(columns) != 3 or not columns[0] or not BLOCK_NAME.fullmatch(columns[1]):
                issues.append(f"{path.name}:{number}: 旧 ID とブロック名を含む 3 列が必要です")
                continue
            old_id, block, target_id = columns
            if old_id in mappings:
                issues.append(f"{path.name}:{number}: 旧 ID の重複 #{old_id}")
                continue
            mappings[old_id] = (block, target_id)
        return mappings


def heading_urls(source: Path, base_url: str) -> dict[str, str]:
    """ブロックの明示 ID を, 所属するページの URL に対応させる."""
    urls = {}
    page_url = f"{base_url}/index.html"
    fence = ""
    for line in source.read_text(encoding="utf-8").splitlines():
        marker = FENCE.match(line)
        if fence:
            if marker:
                run, rest = marker.groups()
                if run.startswith(fence) and not rest.strip():
                    fence = ""
            continue
        if marker:
            fence = marker[1]
            continue
        heading = HEADING.match(line)
        if not heading:
            continue
        heading_id = HEADING_ID.search(heading[2])
        if not heading_id:
            continue
        fragment = heading_id[1]
        encoded = quote(fragment, safe="")
        if heading[1] == "#":
            page_url = f"{base_url}/{encoded}.html"
            urls[fragment] = page_url
        else:
            urls[fragment] = f"{page_url}#{encoded}"
    return urls


def main() -> int:
    repo = Path(__file__).resolve().parents[1]
    docs = Path(sys.argv[1]) if len(sys.argv) > 1 else repo / "docs"
    old_html = docs / "lectures" / "dsp5.html"
    table = repo / "scripts" / "reg_old_anchors.tsv"
    issues: list[str] = []
    try:
        mappings = read_mappings(table, issues)
    except OSError as exc:
        print(exc, file=sys.stderr)
        return 1

    collector = RefCollector()
    if old_html.is_file():
        collector.feed(old_html.read_text(encoding="utf-8"))
        collector.close()
        for old_id in sorted(mappings.keys() - collector.ids):
            issues.append(f"{old_html}: 旧 ID がありません #{old_id}")
    else:
        issues.append(f"{old_html}: 旧ページの HTML がありません")

    for source in sorted((repo / "lectures").rglob("*.md")):
        if source == repo / "lectures" / "dsp" / "dsp5.md":
            continue
        for number, line in enumerate(source.read_text(encoding="utf-8").splitlines(), start=1):
            for match in DSP5_LINK.finditer(line):
                old_id = unquote(match[1])
                if old_id not in mappings:
                    issues.append(f"{source.relative_to(repo)}:{number}: 対応表にない旧 ID #{old_id}")

    block_urls: dict[str, dict[str, str]] = {}
    rows = []
    for old_id, (block, target_id) in mappings.items():
        source = Path("fields") / "regression" / f"{block}.md"
        base_url = source.with_suffix("").as_posix()
        if not (repo / source).is_file():
            issues.append(f"#{old_id}: 行き先のブロックがありません {source}")
            continue
        if target_id:
            if block not in block_urls:
                block_urls[block] = heading_urls(repo / source, base_url)
            url = block_urls[block].get(target_id)
            if url is None:
                issues.append(f"#{old_id}: 行き先の見出し ID がありません {source}#{target_id}")
                continue
        else:
            url = f"{base_url}/index.html"
        rows.append(f"{old_id}\t{block}\t{target_id}\t{url}")

    if issues:
        for issue in issues:
            print(issue, file=sys.stderr)
        return 1
    print(f"{TSV_HEADER}\turl")
    for row in rows:
        print(row)
    return 0


if __name__ == "__main__":
    sys.exit(main())
