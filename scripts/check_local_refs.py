#!/usr/bin/env python3
"""生成した HTML のサイト内参照 (画像, 動画, リンク, CSS, script) の参照先が実在するかを調べる.

使い方: python3 scripts/check_local_refs.py [docs]
参照切れがあれば, ページと参照を列挙して終了コード 1 を返す.
ページ内の anchor (#id) と外部 URL は調べない.
"""

import sys
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlparse

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
        self.refs: list[str] = []

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        for name, value in attrs:
            if value and (tag, name) in REF_ATTRS:
                self.refs.append(value)


def local_target(root: Path, page: Path, ref: str) -> Path | None:
    """サイト内の参照なら参照先の path を返す. 外部 URL と anchor だけの参照は None."""
    url = urlparse(ref)
    if url.scheme or url.netloc:
        return None
    path = unquote(url.path)
    if not path:
        return None
    target = root / path.lstrip("/") if path.startswith("/") else page.parent / path
    if target.is_dir():
        target = target / "index.html"
    return target


def broken_refs(root: Path) -> list[tuple[Path, str]]:
    broken = []
    for page in sorted(root.rglob("*.html")):
        collector = RefCollector()
        collector.feed(page.read_text(encoding="utf-8", errors="replace"))
        for ref in collector.refs:
            target = local_target(root, page, ref)
            if target is not None and not target.exists():
                broken.append((page.relative_to(root), ref))
    return broken


def main() -> int:
    root = Path(sys.argv[1] if len(sys.argv) > 1 else "docs").resolve()
    if not root.is_dir():
        print(f"check_local_refs: {root} がありません", file=sys.stderr)
        return 2
    broken = broken_refs(root)
    for page, ref in broken:
        print(f"{page}\t{ref}")
    if broken:
        print(f"check_local_refs: 参照切れ {len(broken)} 件", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
