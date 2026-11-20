#!/usr/bin/env python3
"""生成した HTML のサイト内参照 (画像, 動画, リンク, CSS, script) の参照先が実在するかを調べる.

使い方: python3 scripts/check_local_refs.py [docs]
参照切れがあれば, ページと参照を列挙して終了コード 1 を返す.
サイト内の <a href> は, 同じページと別のページの fragment (#id) も調べる.
空の fragment と外部 URL は調べない.
"""

import sys
from functools import cache
from html.parser import HTMLParser
from pathlib import Path
from typing import NamedTuple
from urllib.parse import ParseResult, unquote, urlparse

# (tag, 属性) の組のうち, 参照先の file を指すもの.
REF_ATTRS = {
    ("img", "src"),
    ("source", "src"),
    ("video", "src"),
    ("video", "poster"),
    ("audio", "src"),
    ("embed", "src"),
    ("object", "data"),
    ("a", "href"),
    ("link", "href"),
    ("script", "src"),
}


class RefCollector(HTMLParser):
    def __init__(self) -> None:
        super().__init__()
        self.refs: list[tuple[str, str]] = []
        self.ids: set[str] = set()

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        for name, value in attrs:
            if not value:
                continue
            if (tag, name) in REF_ATTRS:
                self.refs.append((tag, value))
            if name == "id" or (tag == "a" and name == "name"):
                self.ids.add(value)


def local_target(root: Path, page: Path, url: ParseResult) -> Path | None:
    """サイト内の参照先を返す. path が空なら同じページを指す."""
    if url.scheme or url.netloc:
        return None
    path = unquote(url.path)
    if not path:
        return page
    target = root / path.lstrip("/") if path.startswith("/") else page.parent / path
    if target.is_dir():
        target = target / "index.html"
    return target


class BrokenRef(NamedTuple):
    page: Path
    ref: str
    missing_id: bool


def broken_refs(root: Path) -> list[BrokenRef]:
    @cache
    def collect(path: Path) -> RefCollector:
        collector = RefCollector()
        collector.feed(path.read_text(encoding="utf-8", errors="replace"))
        collector.close()
        return collector

    broken = []
    for page in sorted(root.rglob("*.html")):
        for tag, ref in collect(page.resolve()).refs:
            url = urlparse(ref)
            target = local_target(root, page, url)
            if target is None:
                continue
            if not target.exists():
                broken.append(BrokenRef(page.relative_to(root), ref, missing_id=False))
            elif tag == "a" and url.fragment:
                if unquote(url.fragment) not in collect(target.resolve()).ids:
                    broken.append(BrokenRef(page.relative_to(root), ref, missing_id=True))
    return broken


def main() -> int:
    root = Path(sys.argv[1] if len(sys.argv) > 1 else "docs").resolve()
    if not root.is_dir():
        print(f"check_local_refs: {root} がありません", file=sys.stderr)
        return 2
    broken = broken_refs(root)
    for issue in broken:
        print(f"{issue.page}\t{issue.ref}")
    if broken:
        missing_ids = sum(issue.missing_id for issue in broken)
        missing_files = len(broken) - missing_ids
        print(
            f"check_local_refs: 参照切れ {len(broken)} 件 "
            f"(file なし {missing_files} 件, ID なし {missing_ids} 件)",
            file=sys.stderr,
        )
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
