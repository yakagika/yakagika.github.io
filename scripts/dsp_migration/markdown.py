"""Locate Markdown edits in source and confirm that Pandoc reads them as intended."""

from __future__ import annotations

from collections import Counter
from dataclasses import dataclass
from functools import lru_cache
from html import unescape
from html.parser import HTMLParser
import json
import re
import subprocess


class MigrationError(ValueError):
    pass


@lru_cache(maxsize=256)
def parse(text: str) -> list:
    return pandoc_document(text)["blocks"]


def pandoc_document(text: str, reader: str = "markdown", *, metadata_file=None) -> dict:
    command = ["pandoc", f"--from={reader}", "--to=json"]
    if metadata_file is not None:
        command.append(f"--metadata-file={metadata_file}")
    result = subprocess.run(
        command,
        input=text, text=True, capture_output=True, encoding="utf-8", check=True,
    )
    return json.loads(result.stdout)


def nodes(value):
    if isinstance(value, dict):
        if "t" in value:
            yield value
        for item in value.values():
            yield from nodes(item)
    elif isinstance(value, list):
        for item in value:
            yield from nodes(item)


def front_matter(text: str) -> tuple[str, str]:
    match = re.match(r"\A---\n(.*?)^--- *\n", text, re.DOTALL | re.MULTILINE)
    if not match:
        raise MigrationError("front matter がありません")
    return match[1], text[match.end():]


# These locate candidates only. The marked Pandoc parse selects actual headings and URLs.
LIST_PREFIX = r"[ \t]*(?:(?:[-+*]|[0-9]+[.)])[ \t]+)?"
ATX = re.compile(rf"^({LIST_PREFIX})(#{{1,6}})[ \t]+(?=\S)")
SETEXT = re.compile(r"^[ \t]*(?:-{2,}|=+)[ \t]*$")
ATTR_TAIL = re.compile(r"\s*({[^{}]*})\s*$")
ATTR_TOKEN = re.compile(r'''[^\s"'=]+(?:=(?:"[^"]*"|'[^']*'|[^\s]+))?''')
PREFIX = "dspmigrationheadinglocation"
URL_PREFIX = "dspmigrationurllocation"
ADVANCED = re.compile(r" *\(発展\)")
ADVANCED_LABEL = re.compile(r" *\(発展\)(?=\][ \t]*\(\s*<?\Z)")
URL_CANDIDATE = re.compile(
    r'''(?P<md>\]\(\s*<?|^ {0,3}\[[^\]\n]+\]:[ \t]*<?)(?P<target>[^\s()<>"']+)'''
    r'''|\b(?P<attr>href|src|poster|data)\s*=\s*(?:"(?P<dq>[^"]+)"|'(?P<sq>[^']+)'|(?P<bare>[^\s>]+))''',
    re.MULTILINE | re.IGNORECASE,
)
TAG_ATTR = re.compile(r'''([^\s=/>]+)(?:\s*=\s*(?:"([^"]*)"|'([^']*)'|([^\s>]+)))?''')
URL_ATTRS = {
    ("a", "href"), ("img", "src"), ("source", "src"), ("video", "src"),
    ("video", "poster"), ("audio", "src"), ("iframe", "src"), ("embed", "src"),
    ("object", "data"), ("link", "href"), ("script", "src"),
}
FENCE = re.compile(r"^[ \t]*(`{3,}|~{3,})([^\n]*)$")


def apply_edits(text: str, edits: list[tuple[int, int, str]]) -> str:
    pos, parts = 0, []
    for start, end, value in sorted(edits):
        if start < pos:
            raise MigrationError(f"変換範囲が重複しています (offset {start})")
        parts.extend((text[pos:start], value))
        pos = end
    parts.append(text[pos:])
    return "".join(parts)


class RawTags(HTMLParser):
    def __init__(self, html: str):
        super().__init__(convert_charrefs=False)
        self.offsets = line_offsets(html)
        self.ids = []
        self.urls = []
        self.answers = 0
        self.feed(html)
        self.close()

    def handle_starttag(self, tag, attrs):
        actual = dict(attrs)
        if actual.get("id"):
            self.ids.append(actual["id"])
        if tag == "a" and actual.get("name"):
            self.ids.append(actual["name"])
        if tag == "details" and "protected" in (actual.get("class") or "").split():
            self.answers += 1
        raw = self.get_starttag_text()
        line, offset = self.getpos()
        start = self.offsets[line - 1] + offset
        for match in TAG_ATTR.finditer(raw, len(tag) + 1):
            attr = match[1].lower()
            if (tag, attr) not in URL_ATTRS or not actual.get(attr):
                continue
            group = next((i for i in (2, 3, 4) if match[i] is not None), None)
            if group is not None:
                self.urls.append((start + match.start(group), start + match.end(group), attr, unescape(match[group])))


def line_offsets(text: str) -> list[int]:
    return [0, *(m.end() for m in re.finditer("\n", text))]


def fenced_code(text: str) -> list[str]:
    """Read the literal fence contents, including blank lines Pandoc may discard."""
    result, opening, lines = [], None, []
    for line in text.splitlines(keepends=True):
        match = FENCE.fullmatch(line.rstrip("\n"))
        if opening is None:
            if match:
                opening, info = match.groups()
                lines = [line]
            continue
        lines.append(line)
        if match and match[1][0] == opening[0] and len(match[1]) >= len(opening) and not match[2].strip():
            if not info.strip().startswith("{="):
                result.append("".join(lines))
            opening = None
    return result


def raw_content(node):
    return node["t"] in {"RawBlock", "RawInline"} and node["c"][0] == "html"


def attrs(node):
    value = node.get("c")
    kind = node["t"]
    if kind == "Header":
        yield value[1]
    elif kind in {"Div", "Span", "Link", "Image", "Code", "CodeBlock", "Figure", "Table"}:
        yield value[0]
        if kind == "Table":
            # TableHead, TableBody, TableFoot, Row and Cell have untagged attributes.
            _, _, _, head, bodies, foot = value
            for part in [head, *bodies, foot]:
                yield part[0]
                for rows in (part[1],) if len(part) == 2 else (part[2], part[3]):
                    for row in rows:
                        yield row[0]
                        for cell in row[1]:
                            yield cell[0]


def identifiers(value) -> list[str]:
    result = []
    for node in nodes(value):
        result.extend(attr[0] for attr in attrs(node) if attr[0])
        if raw_content(node):
            result.extend(RawTags(node["c"][1]).ids)
    return result


def without_advanced(inlines, *, suffix_only: bool = False):
    """Remove the literal label, preserving other inline nodes and their attributes."""
    result = []
    for i, token in enumerate(inlines):
        text = token.get("c") if token["t"] == "Str" else None
        if text and (not suffix_only or i == len(inlines) - 1):
            stripped = re.sub(r"\(発展\)$" if suffix_only else r"\(発展\)", "", text)
            if stripped != text:
                if stripped:
                    result.append({"t": "Str", "c": stripped})
                elif result and result[-1]["t"] in {"Space", "SoftBreak"}:
                    result.pop()
                continue
        result.append(token)
    return result


@dataclass(frozen=True)
class Heading:
    id: str
    start: int
    end: int
    top_index: int | None
    prefix: str
    title_start: int


def locate_headings(text: str, blocks: list) -> dict[str, Heading]:
    if PREFIX in text:
        raise MigrationError("見出し位置の検査用文字列が原稿にあります")
    offsets = line_offsets(text)
    lines = text.split("\n")
    candidates = {}
    edits = []
    for i, line in enumerate(lines):
        atx = ATX.match(line)
        if atx:
            title_start, prefix, stop = atx.end(), atx[1], i + 1
        elif line.strip() and i + 1 < len(lines) and SETEXT.fullmatch(lines[i + 1]):
            prefix = re.match(LIST_PREFIX, line)[0]
            title_start, stop = len(prefix), i + 2
        else:
            continue
        marker = f"{PREFIX}{i}q"
        candidates[marker] = (i, stop, prefix, title_start)
        position = offsets[i] + title_start
        edits.append((position, position, marker + " "))

    original = [node for node in nodes(blocks) if node["t"] == "Header"]
    indexed = {id(node): i for i, node in enumerate(blocks) if node["t"] == "Header"}
    marked = [node for node in nodes(parse(apply_edits(text, edits))) if node["t"] == "Header"]
    if len(original) != len(marked):
        raise MigrationError(f"見出しと原稿の行が対応しません ({len(original)} 件 / {len(marked)} 件)")
    result = {}
    for old, new in zip(original, marked, strict=True):
        level, attr, label = old["c"]
        new_level, new_attr, new_label = new["c"]
        marker = new_label[0].get("c") if new_label and new_label[0]["t"] == "Str" else None
        if (marker not in candidates or (level, attr[1:], label) != (new_level, new_attr[1:], new_label[2:])
                or new_label[1]["t"] != "Space"):
            raise MigrationError(f"見出しの原稿位置を確認できません #{attr[0]}")
        start, stop, prefix, title = candidates[marker]
        if attr[0] in result:
            raise MigrationError(f"見出し ID が重複しています #{attr[0]}")
        result[attr[0]] = Heading(attr[0], offsets[start], offsets[stop] if stop < len(offsets) else len(text),
                                  indexed.get(id(old)), prefix, title)
    return result


@dataclass(frozen=True)
class URL:
    start: int
    end: int
    url: str
    attribute: str = ""
    label_start: int | None = None
    label_end: int | None = None


def locate_urls(text: str, blocks: list) -> list[URL]:
    if URL_PREFIX in text:
        raise MigrationError("リンク位置の検査用文字列が原稿にあります")
    candidates, edits = {}, []
    for i, match in enumerate(URL_CANDIDATE.finditer(text)):
        group = next(name for name in ("target", "dq", "sq", "bare") if match[name] is not None)
        start, end = match.span(group)
        marker = f"{URL_PREFIX}{i}q"
        candidates[marker] = (start, end)
        # Keep the original path/scheme/fragment so that the parser still sees the same syntax.
        edits.append((end, end, marker))
    used = {}
    observed = Counter()

    def record(url, attr="", label=()):
        marker = re.search(rf"{URL_PREFIX}[0-9]+q$", url)
        if marker is None:
            return
        token = marker[0]
        start, end = candidates[token]
        label_start = label_end = None
        if not attr and label and without_advanced(label, suffix_only=True) != label:
            line_start = text.rfind("\n", 0, start) + 1
            suffix = ADVANCED_LABEL.search(text[line_start:start])
            if suffix is None:
                raise MigrationError(f"リンクの (発展) を原稿で特定できません ({url[:marker.start()]})")
            label_start, label_end = (line_start + pos for pos in suffix.span())
        used[token] = URL(start, end, url[:marker.start()], attr, label_start, label_end)
        observed[(attr, url[:marker.start()])] += 1

    for node in nodes(parse(apply_edits(text, edits))):
        if node["t"] in {"Link", "Image"}:
            record(node["c"][2][0], label=node["c"][1] if node["t"] == "Link" else [])
        elif raw_content(node):
            for _, _, attr, url in RawTags(node["c"][1]).urls:
                record(url, attr)

    for node in nodes(blocks):
        if node["t"] in {"Link", "Image"}:
            url = node["c"][2][0]
            label = node["c"][1]
            if (is_local_relative(url) or (node["t"] == "Link" and without_advanced(label, suffix_only=True) != label)):
                if observed[("", url)] < 1:
                    raise MigrationError(f"原稿でリンク先を特定できません ({url})")
                observed[("", url)] -= 1
        elif raw_content(node):
            for _, _, attr, url in RawTags(node["c"][1]).urls:
                if is_local_relative(url):
                    if observed[(attr, url)] < 1:
                        raise MigrationError(f"原稿で HTML のリンク先を特定できません ({url})")
                    observed[(attr, url)] -= 1
    return list(used.values())


def is_local_relative(url: str) -> bool:
    return bool(url and not url.startswith("/") and not re.match(r"^[a-zA-Z][a-zA-Z0-9+.-]*:", url))
