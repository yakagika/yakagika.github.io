#!/usr/bin/env python3
"""Generate and verify DSP textbook blocks and course materials from the legacy chapters."""

from __future__ import annotations

import argparse
from collections import Counter
import csv
from dataclasses import dataclass, field
from functools import cache
from hashlib import sha256
from html import escape
import json
from pathlib import Path
import posixpath
import re
import subprocess
import sys
import tempfile
from urllib.parse import unquote, urlparse, urlsplit, urlunsplit

from course import COURSE_YAML, MATERIALS, REGRESSION, entry, material_path, verify_course
from markdown import (
    ADVANCED, ATTR_TAIL, ATTR_TOKEN, MigrationError, RawTags, apply_edits,
    fenced_code, front_matter, identifiers, locate_headings, locate_urls, nodes, parse,
    pandoc_document, raw_content, without_advanced,
)


ROOT = Path(__file__).resolve().parents[2]
TABLES = Path(__file__).resolve().parent
CHAPTERS = (2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16)
ASCII_ID = re.compile(r"[a-z0-9-]+")
RESERVED = {"index", "exercises"}
EXERCISE = re.compile(r"exercise-dsp[0-9]+-[0-9]+")
PYTHON = {"python", "py", "python3"}
FIELDS = {
    "analysis-design": ("分析設計", "目的, データ, 手法を組み合わせて分析を設計する方法の教科書です."),
    "descriptive": ("記述統計", "データを図と数値で要約し, 読み取る方法の教科書です."),
    "machine-learning": ("機械学習", "分類, クラスタリング, パターン発見, ニューラルネットワークの教科書です."),
    "deep-learning": ("深層学習とその応用", "Transformer, 自然言語処理, 画像認識の教科書です."),
    "bayes": ("ベイズ統計学", "ベイズ統計学による統計モデリングの教科書です."),
    "causal": ("因果推論", "因果推論の教科書です."),
    "survey": ("調査データの扱い", "質問紙調査と欠損値補定の教科書です."),
}
LAYOUT_COLUMNS = (
    "field", "block", "title", "chapter", "ranges", "session", "level",
    "requires", "exercises", "generate", "open", "kind", "copy",
)
WARNING_COPY = ("dl-training-procedure", 10, "opening-warn")
EXERCISE_MOVE = {"exercise-dsp10-2": "dl-training-case"}
LINK_COLUMNS = ("chapter", "block", "section", "context", "label", "target")


def table(path: Path, columns: tuple[str, ...]):
    with path.open(encoding="utf-8", newline="") as source:
        reader = csv.DictReader(source, delimiter="\t")
        if reader.fieldnames != list(columns):
            raise MigrationError(f"{path.name} の列が違います ({', '.join(columns)})")
        for row in reader:
            if None in row or None in row.values():
                raise MigrationError(f"{path.name}:{reader.line_num} の列数が違います")
            yield row


def split_csv(text: str) -> tuple[str, ...]:
    return tuple(part.strip() for part in text.split(",")) if text else ()


def boolean(text, *, default):
    if not text:
        return default
    if text not in {"true", "false"}:
        raise MigrationError(f"true または false を指定してください ({text})")
    return text == "true"


def require_validation_copy(root: Path, option: str):
    root = root.resolve()
    temp_roots = (Path("/tmp").resolve(), Path(tempfile.gettempdir()).resolve())
    if (root == ROOT or (root / ".git").exists()
            or not any(temp in root.parents for temp in temp_roots)):
        raise MigrationError(f"{option} はビルド用の写しにだけ指定できます")


def field_block_names(path: Path) -> list[str]:
    try:
        metadata = pandoc_document("", "markdown-smart", metadata_file=path)["meta"]
    except subprocess.CalledProcessError as error:
        raise MigrationError(f"{path} の YAML を読めません ({error.stderr.strip()})") from error
    blocks = metadata.get("blocks", {})
    if blocks.get("t") != "MetaList":
        raise MigrationError(f"{path} の blocks はブロック名のリストで指定してください")
    names = []
    for item in blocks["c"]:
        inlines = item["c"]
        if (item["t"] != "MetaInlines" or len(inlines) != 1 or inlines[0]["t"] != "Str"
                or not ASCII_ID.fullmatch(inlines[0]["c"])):
            raise MigrationError(f"{path} の blocks に不正なブロック名があります")
        names.append(inlines[0]["c"])
    if len(set(names)) != len(names):
        raise MigrationError(f"{path} の blocks にブロック名の重複があります")
    return names


def lecture_chapter(path):
    absolute = posixpath.normpath(posixpath.join("/lectures", unquote(path)))
    match = re.fullmatch(r"/lectures/dsp([0-9]+)\.html", absolute)
    return int(match[1]) if match else None


@dataclass(frozen=True)
class Segment:
    start: int
    end: int
    nodes: list
    first: str | None


@dataclass(frozen=True)
class AddedLink:
    original: str
    replacement: str
    start: int
    end: int
    section: str
    label: str
    target: str


class Chapter:
    def __init__(self, root: Path, number: int, migrate: bool):
        self.number = number
        self.path = root / f"lectures/dsp/dsp{number}.md"
        text = self.path.read_text(encoding="utf-8")
        self.meta, self.body = front_matter(text)
        self.preceding_lines = text[:len(text) - len(self.body)].count("\n")
        self.ast = parse(self.body)
        self.ids = set(identifiers(self.ast))
        self.headings = locate_headings(self.body, self.ast) if migrate else {}
        self.urls = locate_urls(self.body, self.ast) if migrate else []
        self.boundaries = {
            ident: (h.start, h.top_index) for ident, h in self.headings.items() if h.top_index is not None
        }
        self.boundaries.update({"@start": (0, 0), "@end": (len(self.body), len(self.ast))})
        self.intro = None
        self.exercises = {}
        if migrate:
            self.read_exercises()

    def read_exercises(self):
        names = [name for name in self.boundaries if EXERCISE.fullmatch(name)]
        if not names and "演習" not in self.boundaries:
            return
        if not names or "演習" not in self.boundaries:
            raise MigrationError(f"dsp{self.number} の演習見出しが揃っていません")
        start = self.boundary("演習")
        names.sort(key=self.boundary)
        if start >= self.boundary(names[0]) or any(not x.startswith(f"exercise-dsp{self.number}-") for x in names):
            raise MigrationError(f"dsp{self.number} の演習の順序または番号が違います")
        for i, name in enumerate(names):
            end = names[i + 1] if i + 1 < len(names) else "@end"
            exercise = self.segment(name, end)
            if (len(exercise.nodes) < 2 or
                    any(node["t"] == "Header" and node["c"][0] <= 2 for node in exercise.nodes[1:])):
                raise MigrationError(f"dsp{self.number} の演習範囲が不正です (#{name})")
            self.exercises[name] = exercise
        self.intro = self.segment("演習", names[0])

    def boundary(self, ident):
        try:
            return self.boundaries[ident]
        except KeyError:
            raise MigrationError(f"dsp{self.number} の最上位に範囲の見出しがありません (#{ident})") from None

    def segment(self, first, last):
        start, start_index = self.boundary(first)
        end, end_index = self.boundary(last)
        if start >= end or start_index > end_index:
            raise MigrationError(f"dsp{self.number} の範囲が逆順または空です ({first}..{last})")
        return Segment(start, end, self.ast[start_index:end_index], None if first == "@start" else first)

    def source_line(self, offset):
        return self.preceding_lines + self.body.count("\n", 0, offset) + 1

    def opening_warning(self):
        match = re.match(r"\s*(::: warn\n.*?^:::[ \t]*\n)", self.body, re.DOTALL | re.MULTILINE)
        if not match or parse(match[1]) != self.ast[:1]:
            raise MigrationError(f"dsp{self.number} の冒頭の warn を特定できません")
        return Segment(*match.span(1), self.ast[:1], None)


@dataclass
class Block:
    name: str
    field: str
    title: str
    chapter: Chapter
    session: str
    level: str
    requires: tuple[str, ...]
    ranges: list[Segment]
    exercises: tuple[str, ...]
    generate: bool
    open: bool
    kind: str
    warning: Segment | None
    edits: dict = field(default_factory=dict)
    pages: list[str] = field(default_factory=lambda: ["index"])
    anchors: dict[str, str] = field(default_factory=dict)
    all_ids: dict[str, str] = field(default_factory=dict)
    sections: list[dict] = field(default_factory=list)

    def pieces(self):
        if self.warning:
            yield self.warning, "warning"
        for segment in self.ranges:
            yield segment, "body"
        if self.exercises:
            yield self.chapter.intro, "intro"
            for name in self.exercises:
                yield self.chapter.exercises[name], "exercise"

    def path(self):
        return Path("fields", self.field, f"{self.name}.md")

    def page_path(self, page):
        return f"/fields/{self.field}/{self.name}/{page}.html"


def parse_ranges(chapter, specification):
    result = []
    for pair in split_csv(specification):
        ends = pair.split("..")
        if len(ends) != 2 or not all(ends):
            raise MigrationError(f"dsp{chapter.number} の範囲が不正です ({pair})")
        result.append(chapter.segment(*ends))
    if not result:
        raise MigrationError(f"dsp{chapter.number} の本文の割り当てが空です")
    return result


def summarize_difference(want, got):
    if isinstance(want, list) and isinstance(got, list):
        for i, (left, right) in enumerate(zip(want, got)):
            if left != right:
                return f"[{i}]" + summarize_difference(left, right)
        return f" (要素数 {len(want)} / {len(got)})"
    if isinstance(want, dict) and isinstance(got, dict):
        for key in want.keys() & got.keys():
            if want[key] != got[key]:
                return f".{key}" + summarize_difference(want[key], got[key])
    return f" ({str(want)[:85]!r} / {str(got)[:85]!r})"


class Migration:
    def __init__(self, root, *, layout=TABLES / "layout.tsv", page_ids=TABLES / "page_ids.tsv",
                 regression="legacy", open_all=False):
        if open_all:
            require_validation_copy(root, "--open")
        self.root = root
        self.regression = regression
        self.open_all = open_all
        material_chapters = {n for n, _, _ in MATERIALS.values()}
        self.chapters = {n: Chapter(root, n, n in CHAPTERS or n in material_chapters) for n in range(1, 17)}
        self.materials = {
            name: (self.chapters[n], self.chapters[n].segment(first, "@end"), title)
            for name, (n, first, title) in MATERIALS.items()
        }
        self.material_routes = {}
        for name, (chapter, segment, _) in self.materials.items():
            ids = identifiers(segment.nodes)
            if len(ids) != len(set(ids)):
                raise MigrationError(f"{material_path(name)} の原稿に重複 ID があります")
            self.material_routes.update(((chapter.number, ident), name) for ident in ids)
        self.blocks = {}
        self.legacy_anchors = {}
        self.routes = {}
        self.provisional = []
        self.coverage = {}
        for row in table(layout, LAYOUT_COLUMNS):
            self.read_row(row, open_all)
        self.check_layout()
        mapped_ids = self.read_page_ids(page_ids)
        for block in self.blocks.values():
            self.plan_pages(block, mapped_ids)
        self.regression_ids = {
            row["old_id"]: (row["block"], row["block_id"])
            for row in table(root / "scripts/reg_old_anchors.tsv", ("old_id", "block", "block_id"))
        }
        self.added_links = self.read_added_links()

    def read_added_links(self):
        links = {}
        for row in table(TABLES / "added_links.tsv", LINK_COLUMNS):
            name, section, context, label, target = (
                row[key] for key in ("block", "section", "context", "label", "target")
            )
            block = self.blocks[name]
            chapter = block.chapter
            heading = chapter.headings[section]
            if name in links or str(chapter.number) != row["chapter"] or context.count(label) != 1:
                raise MigrationError(f"{name} の追加リンクの指定が不正です")
            piece = next((part for part in block.ranges if part.start <= heading.start < part.end), None)
            if piece is None:
                raise MigrationError(f"{name} に追加リンクの節がありません (#{section})")
            end = min((loc.start for loc in chapter.headings.values()
                       if heading.start < loc.start < piece.end), default=piece.end)
            start = chapter.body.find(context, heading.end, end)
            if start < 0 or chapter.body.count(context, heading.end, end) != 1:
                raise MigrationError(f"dsp{chapter.number}#{section} の追加リンクの原文が一意に見つかりません")
            paragraph_start = max(chapter.body.rfind("\n\n", heading.end, start), heading.end)
            paragraph_end = chapter.body.find("\n\n", start, end)
            if chapter.body.count(label, paragraph_start, end if paragraph_end < 0 else paragraph_end) != 1:
                raise MigrationError(f"dsp{chapter.number}#{section} の追加リンクの文字列が一意に見つかりません")
            dest, _, anchor = target.removeprefix("block:").partition("#")
            if not target.startswith("block:") or dest not in self.blocks or (
                anchor and anchor not in self.blocks[dest].all_ids
            ):
                raise MigrationError(f"{name} の追加リンクの行き先がありません ({target})")
            links[name] = AddedLink(
                context, context.replace(label, f"[{label}]({target})"),
                start, start + len(context), section, label, target,
            )
        if len(links) != 3:
            raise MigrationError(f"追加リンクの表に {len(links)} 件あります (期待値 3)")
        return links

    def read_row(self, row, open_all):
        name = row["block"]
        if name in self.blocks or not ASCII_ID.fullmatch(name) or row["field"] not in FIELDS:
            raise MigrationError(f"分野またはブロック名が不正または重複しています ({row['field']}/{name})")
        if row["level"] not in {"required", "advanced"} or not re.fullmatch(r"S|[0-9]+", row["session"]):
            raise MigrationError(f"回または必修・発展の指定が不正です ({name})")
        number = int(row["chapter"])
        if number not in CHAPTERS:
            raise MigrationError(f"dsp{number} はブロックへの移行対象外です")
        chapter = self.chapters[number]
        kind, copy = row["kind"], row["copy"]
        if kind not in {"", "theory", "code"} or (copy and (name, number, copy) != WARNING_COPY):
            raise MigrationError(f"{name} の kind または複製指定が不正です")
        exercises = split_csv(row["exercises"])
        if any(exercise not in chapter.exercises for exercise in exercises):
            raise MigrationError(f"{name} の演習 ID が dsp{number} にありません")
        self.blocks[name] = Block(
            name, row["field"], row["title"], chapter, row["session"], row["level"],
            split_csv(row["requires"]), parse_ranges(chapter, row["ranges"]),
            tuple(sorted(exercises, key=chapter.boundary)),
            boolean(row["generate"], default=True), open_all or boolean(row["open"], default=False),
            kind, chapter.opening_warning() if copy else None,
        )

    def check_layout(self):
        dedicated = {n: (name, first) for name, (n, first, _) in MATERIALS.items()}
        for n, chapter in self.chapters.items():
            if n not in CHAPTERS:
                if n in dedicated:
                    self.coverage[n] = (len(chapter.body.splitlines()), 0)
                    continue
                self.coverage[n] = (0, chapter.body.count("\n") + bool(chapter.body and not chapter.body.endswith("\n")))
                self.legacy_anchors[n] = sorted(chapter.ids)
                continue
            owners = []
            for block in self.blocks.values():
                if block.chapter.number != n:
                    continue
                owners.extend((segment.start, segment.end, block.name) for segment in block.ranges)
                for exercise in block.exercises:
                    segment = chapter.exercises[exercise]
                    owners.append((segment.start, segment.end, block.name))
                if block.level == "required":
                    for segment in block.ranges:
                        for node in nodes(segment.nodes):
                            if node["t"] == "Header" and without_advanced(node["c"][2]) != node["c"][2]:
                                raise MigrationError(f"必修の {block.name} に発展の見出しがあります (#{node['c'][1][0]})")
            if not owners:
                raise MigrationError(f"dsp{n} の割り当てがありません")
            if chapter.intro:
                intro = chapter.intro
                owners.append((intro.start, intro.end, "演習の導入"))
            if n in dedicated:
                name, _ = dedicated[n]
                _, segment, _ = self.materials[name]
                owners.append((segment.start, segment.end, name))
            end = 0
            for start, stop, owner in sorted(owners):
                if start != end:
                    error = "欠落" if start > end else "重複"
                    raise MigrationError(f"dsp{n}:{chapter.source_line(min(start, end))} に割り当ての{error}があります ({owner})")
                end = stop
            if end != len(chapter.body):
                raise MigrationError(f"dsp{n}:{chapter.source_line(end)} から章末まで割り当てが欠落しています")
            self.coverage[n] = (len(chapter.body.splitlines()), 0)

        catalog = self.blocks.keys() | {p.stem for p in (self.root / "fields/regression").glob("*.md")}
        seen = set()
        for block in self.blocks.values():
            if unknown := set(block.requires) - catalog:
                raise MigrationError(f"{block.name} の前提が未定義です ({', '.join(sorted(unknown))})")
            same_field = {name for name in block.requires
                          if name in self.blocks and self.blocks[name].field == block.field}
            if missing := same_field - seen:
                raise MigrationError(f"{block.name} の前提が分野内の順序で未履修です ({', '.join(sorted(missing))})")
            seen.add(block.name)

    def read_page_ids(self, path):
        mapped = {}
        if path.exists():
            for row in table(path, ("chapter", "old_id", "page_id")):
                chapter = int(row["chapter"].removeprefix("dsp"))
                key, new = (chapter, row["old_id"]), row["page_id"]
                if not ASCII_ID.fullmatch(new) or new in RESERVED:
                    raise MigrationError(f"page_ids.tsv のページ ID が不正または予約済みです ({new})")
                if key in mapped or chapter not in CHAPTERS or key[1] not in self.chapters[chapter].headings:
                    raise MigrationError(f"page_ids.tsv の行が重複または旧 ID がありません (dsp{chapter}#{key[1]})")
                mapped[key] = new
        return mapped

    def plan_pages(self, block, mapped):
        chapter = block.chapter
        shift = int(sum(node["t"] == "Header" and node["c"][0] == 1
                        for part in block.ranges for node in part.nodes) <= 1)
        page = "index"
        lead = next((part.start + re.search(r"\S", chunk).start()
                     for part in block.ranges if (chunk := chapter.body[part.start:part.end]).strip()), None)
        for segment, kind in block.pieces():
            first_level = segment.nodes[0]["c"][0] if segment.first else 0
            if not segment.first and segment.nodes and segment.nodes[0]["t"] != "Header":
                entry = {"old_id": None, "new_id": None, "page": page}
                if kind == "warning":
                    entry["copy"] = "opening-warn"
                block.sections.append(entry)
            for top in segment.nodes:
                for node in nodes(top):
                    if node["t"] != "Header":
                        continue
                    depth, attr, _ = node["c"]
                    old = attr[0]
                    location = chapter.headings[old]
                    if kind == "intro" and old == "演習":
                        level, new = 1, "exercises"
                    elif kind in {"intro", "exercise"}:
                        level, new = (2 if EXERCISE.fullmatch(old) else depth), old
                    else:
                        # Only the first heading of a cut-out subsection is promoted.
                        offset = first_level - 1 if location.start == segment.start and first_level >= 3 else shift
                        level = max(1, depth - offset) if location.top_index is not None else min(6, depth + 1)
                        if level == 1 and location.start + len(location.prefix) == lead:
                            level = 2
                        new = self.page_id(block, old, mapped) if level == 1 else old
                    if level == 1 and location.top_index is not None:
                        if new in block.pages:
                            raise MigrationError(f"{block.name} のページ ID が重複しています ({new})")
                        page = new
                        block.pages.append(page)
                    block.edits[old] = (level, new)
                    if location.top_index is not None:
                        block.sections.append({"old_id": old, "new_id": new, "page": page})
                for old in identifiers(top):
                    if old in block.anchors:
                        raise MigrationError(f"{block.name} の旧 ID が重複しています (#{old})")
                    block.anchors[old] = page
                    if block.generate:
                        self.routes.setdefault((chapter.number, old), []).append((block, page))
        block.all_ids = dict(block.anchors)
        for old, (_, new) in block.edits.items():
            if old != new:
                if new in block.all_ids:
                    raise MigrationError(f"{block.name} のページ ID が本文の ID と重複しています ({new})")
                block.all_ids[new] = block.anchors[old]

    def page_id(self, block, old, mapped):
        key = block.chapter.number, old
        if key in mapped:
            return mapped[key]
        if ASCII_ID.fullmatch(old) and old not in RESERVED:
            new = old
        else:
            digest = sha256(f"{key[0]}\0{old}".encode()).hexdigest()[:16]
            new = f"sec-{digest}"
        self.provisional.append((block.name, old, new))
        return new

    def target(self, block, url, attribute="", *, material=None):
        parts = urlsplit(url)
        if parts.scheme or parts.netloc or not url:
            return url
        path, query, raw_id = parts.path, parts.query, parts.fragment
        chapter = self.materials[material][0].number if material else block.chapter.number
        if path:
            if path.startswith("/"):
                absolute = path
            else:
                absolute = posixpath.normpath(posixpath.join("/lectures", path))
            chapter = lecture_chapter(path)
            if chapter is None:
                return urlunsplit(("", "", absolute, query, raw_id))
        elif not raw_id:
            return urlunsplit(("", "", f"/lectures/dsp{chapter}.html", query, ""))
        old_url = urlunsplit(("", "", f"/lectures/dsp{chapter}.html", query, raw_id))
        ident = unquote(raw_id)
        if chapter not in self.chapters or (ident and ident not in self.chapters[chapter].ids):
            source = material_path(material) if material else block.name
            raise MigrationError(f"{source} から参照する旧 ID がありません ({old_url})")
        if material:
            whole = {1: "guide", 12: "capstone"}
            name = self.material_routes.get((chapter, ident)) if ident else whole.get(chapter)
            if name and not query:
                if name == material and ident:
                    return f"#{raw_id}"
                if attribute:
                    return f"/courses/dsp/{name}.html" + (f"#{raw_id}" if ident else "")
                return f"material:{name}" + (f"#{raw_id}" if ident else "")
        if not ident:
            return old_url
        if chapter == 5 and self.regression == "blocks" and not query:
            if ident in self.regression_ids:
                name, anchor = self.regression_ids[ident]
                if attribute:
                    return self.block_url(name, anchor)
                return f"block:{name}" + (f"#{anchor}" if anchor else "")
        if chapter not in CHAPTERS or query:
            return old_url
        choices = self.routes.get((chapter, ident), [])
        if not choices:
            return old_url
        dest, page = next((pair for pair in choices if pair[0] is block), choices[0])
        fragment = "#exercises" if ident == "演習" else f"#{ident}"
        if attribute:
            return dest.page_path(page) + fragment
        if dest is block:
            return fragment
        return f"block:{dest.name}" + fragment

    def source_piece(self, block, segment, *, material=None):
        chapter = self.materials[material][0] if material else block.chapter
        start, end = segment.start, segment.end
        original = chapter.body[start:end]
        if block and segment is block.warning:
            return original
        edits = []
        if block and (link := self.added_links.get(block.name)) and start <= link.start < end:
            edits.append((link.start - start, link.end - start, link.replacement))
        for url in chapter.urls:
            if not start <= url.start < end:
                continue
            new = self.target(block, url.url, url.attribute, material=material)
            if new != url.url:
                edits.append((url.start - start, url.end - start, escape(new, quote=True) if url.attribute else new))
            if block and url.label_start is not None:
                edits.append((url.label_start - start, url.label_end - start, ""))
        for location in chapter.headings.values():
            if not start <= location.start < end:
                continue
            if material:
                begin = location.start + location.title_start
                line_end = chapter.body.find("\n", begin, end)
                if line_end < 0:
                    line_end = end
                title = chapter.body[begin:line_end]
                attrs = ATTR_TAIL.search(title)
                if attrs and any(token[0].startswith("#") for token in ATTR_TOKEN.finditer(attrs[1][1:-1])):
                    continue
                if attrs:
                    position = begin + attrs.start(1) + 1 - start
                    edits.append((position, position, f"#{location.id} "))
                else:
                    closing = re.search(r"[ \t]+#+[ \t]*$", title)
                    position = line_end - start
                    if closing:
                        edits.append((begin + closing.start() - start, position, ""))
                        position = begin + closing.start() - start
                    edits.append((position, position, f" {{#{location.id}}}"))
                continue
            level, new = block.edits[location.id]
            begin = location.start + location.title_start
            line_end = chapter.body.find("\n", begin)
            line_end = len(chapter.body) if line_end == -1 else line_end
            prefix_end = location.start + len(location.prefix)
            edits.append((prefix_end - start, begin - start, "#" * level + " "))
            alias = f"[]{{#{location.id}}}" if new != location.id else ""
            if alias:
                edits.append((begin - start, begin - start, alias))
            title = chapter.body[begin:line_end]
            attr = ATTR_TAIL.search(title)
            title_end = attr.start() if attr else len(title.rstrip())
            title_text = title[:title_end]
            closing = re.search(r"[ \t]+#+[ \t]*$", title_text)
            if closing:
                title_end = closing.start()
            # The parsed expected AST independently limits removal to literal heading text.
            for marker in ADVANCED.finditer(title[:title_end]):
                a, z = begin + marker.start() - start, begin + marker.end() - start
                if (a, z, "") not in edits:
                    edits.append((a, z, ""))
            tail = [x[0] for x in ATTR_TOKEN.finditer(attr[1][1:-1]) if not x[0].startswith("#")] if attr else []
            attribute = " {#" + " ".join([new, *tail]) + "}"
            edits.append((begin + title_end - start, line_end - start, attribute))
            if location.end > line_end + 1:
                edits.append((line_end + 1 - start, location.end - start, ""))
        return apply_edits(original, edits)

    def expected(self, block, value, *, material=None):
        if isinstance(value, list):
            return [self.expected(block, item, material=material) for item in value]
        if not isinstance(value, dict):
            return value
        if "t" not in value:
            return {key: self.expected(block, item, material=material) for key, item in value.items()}
        if "c" not in value:
            return value
        kind = value["t"]
        original = value["c"]
        content = self.expected(block, original, material=material)
        if kind == "Header" and block:
            old = original[1][0]
            level, new = block.edits[old]
            content[0], content[1][0] = level, new
            content[2] = without_advanced(content[2])
            if new != old:
                content[2].insert(0, {"t": "Span", "c": [[old, [], []], []]})
        elif kind in {"Link", "Image"}:
            content[2][0] = self.target(block, original[2][0], material=material)
            if kind == "Link" and block:
                content[1] = without_advanced(content[1], suffix_only=True)
        elif raw_content(value):
            edits = []
            for start, end, attribute, url in RawTags(original[1]).urls:
                new = self.target(block, url, attribute, material=material)
                if new != url:
                    edits.append((start, end, escape(new, quote=True)))
            content[1] = apply_edits(original[1], edits)
        return {"t": kind, "c": content}

    def render_block(self, block):
        chunks = [self.source_piece(block, part) for part, _ in block.pieces()]
        body = chunks[0]
        for chunk in chunks[1:]:
            if not re.search(r"\n[ \t]*\n$", body) and not re.match(r"[ \t]*\n", chunk):
                body += "\n" if body.endswith("\n") else "\n\n"
            body += chunk
        if not body.endswith("\n"):
            body += "\n"
        code = any(node["t"] == "CodeBlock" and PYTHON.intersection(node["c"][0][1])
                   for piece, _ in block.pieces() for node in nodes(piece.nodes))
        kind = block.kind or ("code" if code else "theory")
        lines = ["---", f"title: {json.dumps(block.title, ensure_ascii=False)}",
                 f"kind: {kind}", f"open: {str(block.open).lower()}"]
        if block.requires:
            lines.extend(["requires:", *(f"  - {name}" for name in block.requires)])
        lines.append("---")
        return "\n".join(lines) + "\n" + body

    def material_metadata(self, name):
        chapter, _, title = self.materials[name]
        bibliography = re.search(r"^bibliography:[^\n]+", chapter.meta, re.MULTILINE)
        return "\n".join([f"title: {title}", *([bibliography[0]] if bibliography else [])])

    def render_material(self, name):
        _, segment, _ = self.materials[name]
        return f"---\n{self.material_metadata(name)}\n---\n" + self.source_piece(None, segment, material=name)

    def verify_material(self, name, text):
        path = material_path(name)
        chapter, segment, _ = self.materials[name]
        meta, body = front_matter(text)
        if meta.strip() != self.material_metadata(name):
            raise MigrationError(f"{path} の front matter が原稿と一致しません")
        if fenced_code(body) != fenced_code(chapter.body[segment.start:segment.end]):
            raise MigrationError(f"{path} のコードフェンスが原稿と逐語一致しません")
        expected = self.expected(None, segment.nodes, material=name)
        actual = parse(body)
        if actual != expected:
            raise MigrationError(f"{path} の構文木に差があります " + summarize_difference(expected, actual))
        if identifiers(actual) != identifiers(segment.nodes):
            raise MigrationError(f"{path} の ID または順序が旧章と一致しません")

    def outputs(self):
        for name, (title, desc) in FIELDS.items():
            blocks = [block for block in self.blocks.values() if block.field == name]
            field_path = Path("fields", name, "field.yaml")
            existing = self.root / field_path
            assigned = [block.name for block in blocks]
            if existing.exists() or existing.is_symlink():
                if not existing.is_file():
                    raise MigrationError(f"{field_path} が通常のファイルとして読めません")
                listed = field_block_names(existing)
                if missing := set(assigned) - set(listed):
                    raise MigrationError(f"{field_path} に割り当て済みのブロックがありません "
                                         f"({', '.join(sorted(missing))})")
                if [block for block in listed if block in assigned] != assigned:
                    raise MigrationError(f"{field_path} のブロックの相対順が割り当て表と異なります")
            else:
                listed = assigned
                if blocks:
                    yield field_path, (
                        f"title: {json.dumps(title, ensure_ascii=False)}\n"
                        f"description: {json.dumps(desc, ensure_ascii=False)}\nblocks:\n"
                        + "".join(f"  - {block.name}\n" for block in blocks)
                    )
            unexpected = sorted(path.name for path in existing.parent.glob("*.md") if path.stem not in listed)
            if unexpected:
                raise MigrationError(f"{field_path.parent} で割り当て表と blocks の両方にないファイルがあります "
                                     + ", ".join(unexpected))
            for block in blocks:
                if block.generate:
                    yield block.path(), self.render_block(block)
                elif not (self.root / block.path()).is_file():
                    raise MigrationError(f"生成から外したブロックの実体がありません ({block.path()})")
        materials = {material_path(name) for name in self.materials}
        directory = self.root / COURSE_YAML.with_suffix("")
        if extra := sorted(path.relative_to(self.root) for path in directory.rglob("*.md")
                           if path.relative_to(self.root) not in materials):
            raise MigrationError("専用ページの表にないファイルがあります " + ", ".join(map(str, extra)))
        for name in self.materials:
            yield material_path(name), self.render_material(name)

    def verify_body(self, block, text):
        meta, body = front_matter(text)
        if re.findall(r"^open:.*$", meta, re.MULTILINE) != [f"open: {str(block.open).lower()}"]:
            raise MigrationError(f"{block.path()} の open が割り当てと一致しません")
        if link := self.added_links.get(block.name):
            if body.count(link.replacement) != 1:
                raise MigrationError(f"{block.path()} の追加リンクの位置または文字列が一致しません")
            body = body.replace(link.replacement, link.original, 1)
        source = block.chapter.body
        if block.warning and not body.startswith(source[block.warning.start:block.warning.end]):
            raise MigrationError(f"{block.path()} の冒頭の warn が原稿と逐語一致しません")
        original_code = [code for segment, _ in block.pieces()
                         for code in fenced_code(source[segment.start:segment.end])]
        if fenced_code(body) != original_code:
            raise MigrationError(f"{block.path()} のコードフェンスが原稿と逐語一致しません")
        actual = parse(body)
        expected = [item for segment, _ in block.pieces() for item in self.expected(block, segment.nodes)]
        if actual != expected:
            raise MigrationError(f"{block.path()} の構文木に差があります " + summarize_difference(expected, actual))
        page = "index"
        ids = {}
        for item in actual:
            if item["t"] == "Header" and item["c"][0] == 1:
                page = item["c"][1][0]
            for ident in identifiers(item):
                if ident in ids:
                    raise MigrationError(f"{block.path()} に重複 ID があります (#{ident})")
                ids[ident] = page
        if ids != block.all_ids:
            raise MigrationError(f"{block.path()} の ID または分割先が割り当てと一致しません")
        counts = Counter(node["t"] for node in nodes(actual)
                         if node["t"] in {"CodeBlock", "Image", "Math", "Table", "RawBlock", "RawInline"})
        counts["Answer"] = sum(RawTags(node["c"][1]).answers for node in nodes(actual) if raw_content(node))
        counts["Exercise"] = sum(EXERCISE.fullmatch(key) is not None for key in ids)
        return counts

    @cache
    def existing_block_route(self, name):
        """Read the routes of a regression or manually maintained block."""
        block = self.blocks.get(name)
        field_name = block.field if block else "regression"
        path = self.root / "fields" / field_name / f"{name}.md"
        if not ASCII_ID.fullmatch(name) or not path.is_file():
            raise MigrationError(f"リンク先のブロックがありません ({name})")
        _, body = front_matter(path.read_text(encoding="utf-8"))
        page, anchors = "index", {}
        for item in parse(body):
            if item["t"] == "Header" and item["c"][0] == 1:
                page = item["c"][1][0]
            for ident in identifiers(item):
                anchors[ident] = page
        return field_name, anchors

    def block_url(self, name, anchor):
        block = self.blocks.get(name)
        if block and block.generate:
            field_name, anchors = block.field, block.all_ids
        else:
            field_name, anchors = self.existing_block_route(name)
        ident = unquote(anchor)
        if ident and ident not in anchors:
            raise MigrationError(f"ブロックにリンク先の ID がありません ({name}#{anchor})")
        page = anchors[ident] if ident else "index"
        fragment = f"#{anchor}" if anchor else ""
        return f"/fields/{field_name}/{name}/{page}.html{fragment}"

    def verify_markdown_links(self, site_docs):
        # Use the site's reference collector for asset paths and rendered IDs.
        sys.path.insert(0, str(TABLES.parent))
        from check_local_refs import RefCollector, local_target

        @cache
        def collect(path):
            parsed = RefCollector()
            parsed.feed(path.read_text(encoding="utf-8"))
            parsed.close()
            return parsed.ids

        def references(value):
            for node in nodes(value):
                if node["t"] in {"Link", "Image"}:
                    is_link = node["t"] == "Link"
                    yield node["c"][2][0], is_link, is_link
                elif raw_content(node):
                    for _, _, attribute, url in RawTags(node["c"][1]).urls:
                        yield url, attribute == "href", False

        def resolve(url, markdown_link, block_name):
            if markdown_link and url.startswith("block:"):
                name, _, ident = url.removeprefix("block:").partition("#")
                return self.block_url(unquote(name), ident)
            if markdown_link and url.startswith("material:"):
                name, _, ident = url.removeprefix("material:").partition("#")
                return f"/courses/dsp/{name}.html" + (f"#{ident}" if ident else "")
            if markdown_link and block_name and url.startswith("#"):
                return self.block_url(block_name, url[1:])
            return url

        counts = Counter()

        def check(source_path, page_route, value, *, block_name=None):
            page = site_docs / page_route.lstrip("/")
            for source, fragment_is_anchor, markdown_link in references(value):
                parsed = urlparse(resolve(source, markdown_link, block_name))
                path = local_target(site_docs, page, parsed)
                if path is None:
                    counts["external"] += 1
                    continue
                path = path.resolve()
                if not path.is_relative_to(site_docs) or not path.is_file():
                    raise MigrationError(f"{source_path} の参照先ファイルがありません ({source})")
                if parsed.fragment and fragment_is_anchor and unquote(parsed.fragment) not in collect(path):
                    raise MigrationError(f"{source_path} の参照先 HTML に ID がありません ({source})")
                counts["local"] += 1

        for block in self.blocks.values():
            if not block.generate:
                continue
            source_path = block.path()
            _, body = front_matter((self.root / source_path).read_text(encoding="utf-8"))
            page_key = "index"
            for item in parse(body):
                if item["t"] == "Header" and item["c"][0] == 1:
                    page_key = item["c"][1][0]
                check(source_path, block.page_path(page_key), item, block_name=block.name)
        for name in self.materials:
            source_path = material_path(name)
            _, body = front_matter((self.root / source_path).read_text(encoding="utf-8"))
            check(source_path, source_path.with_suffix(".html").as_posix(), parse(body))
        print(f"Markdown の参照照合 サイト内 {counts['local']} 件, 外部 URL の保持 {counts['external']} 件")

    def verify_original_layout(self, path, inventory_path=None):
        inventory = json.loads(inventory_path.read_text(encoding="utf-8")) if inventory_path else {}
        observed = set()
        for line in path.read_text(encoding="utf-8").splitlines():
            if not line or line.startswith("#"):
                continue
            field_name, name, _, n, ranges, session, level, _, exercises = line.split("|")
            block = self.blocks[name]
            chapter = block.chapter
            retained = {f"exercise-dsp{x}" for x in split_csv(exercises)} - EXERCISE_MOVE.keys()
            retained.update(key for key, destination in EXERCISE_MOVE.items() if destination == name)
            if name in observed or (block.field, str(chapter.number), block.session, block.level) != (
                field_name, n, session, level
            ) or set(block.exercises) != retained:
                raise MigrationError(f"基準の割り当てと属性が一致しません ({name})")
            observed.add(name)
            begin, end = chapter.source_line(0), chapter.source_line(len(chapter.body))
            want = set()
            for start, stop in (pair.split("-") for pair in split_csv(ranges)):
                want.update(range(begin if start == "B" else int(start), end if stop == "E" else int(stop)))
            got = {line for piece in block.ranges
                   for line in range(chapter.source_line(piece.start), chapter.source_line(piece.end))}
            if inventory:
                headings = {h["id"]: h for h in inventory[n]["hs"] if h["top"]}
                for exercise in block.exercises:
                    heading = headings[exercise]
                    want.update(range(heading["line"], heading["line"] + heading["n"]))
                    piece = chapter.exercises[exercise]
                    got.update(range(chapter.source_line(piece.start), chapter.source_line(piece.end)))
                if block.exercises:
                    first = min(headings[key]["line"] for key in chapter.exercises)
                    want.update(range(headings["演習"]["line"], first))
                    intro = chapter.intro
                    got.update(range(chapter.source_line(intro.start), chapter.source_line(intro.end)))
            if want != got:
                raise MigrationError(f"基準の割り当てと行集合が一致しません ({name}, {sorted(want ^ got)[:8]})")
        if observed != self.blocks.keys():
            raise MigrationError("基準の割り当てとブロック集合が一致しません")
        scope = "本文・演習・演習の導入の行集合" if inventory else "本文の行集合と演習割り当て"
        print(f"基準の{scope}が一致 {len(observed)} ブロック")

    def write_chapter_links(self, path):
        """List chapter-only link occurrences, including material left in the old chapters."""
        counts = Counter()
        with path.open("w", encoding="utf-8", newline="") as output:
            writer = csv.writer(output, delimiter="\t", lineterminator="\n")
            writer.writerow(("source_chapter", "line", "blocks", "old_target", "target"))
            for chapter in self.chapters.values():
                owned = [(b.name, piece) for b in self.blocks.values()
                         if b.chapter is chapter and b.generate for piece, _ in b.pieces()]
                for link in chapter.urls or locate_urls(chapter.body, chapter.ast):
                    parts = urlsplit(link.url)
                    if parts.scheme or parts.netloc or parts.fragment:
                        continue
                    n = lecture_chapter(parts.path)
                    if n not in self.chapters:
                        continue
                    blocks = [name for name, piece in owned if piece.start <= link.start < piece.end]
                    target = urlunsplit(("", "", f"/lectures/dsp{n}.html", parts.query, ""))
                    writer.writerow((chapter.number, chapter.source_line(link.start), ",".join(blocks), link.url, target))
                    counts["all"] += 1
                    counts["moved"] += bool(blocks)
        print(f"章だけを指すリンク {counts['all']} 件 (移行元の行にあるもの {counts['moved']} 件)")

    def validation_visibility(self, *, check):
        if not self.open_all:
            return {}
        sources = [COURSE_YAML, *(p.relative_to(self.root) for p in
                                 sorted((self.root / "fields/regression").glob("*.md")))]
        changed = {}
        for path in sources:
            text = (self.root / path).read_text(encoding="utf-8")
            pattern = r"^open: (?:false|true)$"
            if path != COURSE_YAML:
                meta, _ = front_matter(text)
            else:
                meta = text
            if len(re.findall(pattern, meta, re.MULTILINE)) != 1:
                raise MigrationError(f"{path} の open の設定を一意に特定できません")
            opened = re.sub(pattern, "open: true", text, count=1, flags=re.MULTILINE)
            if check and opened != text:
                raise MigrationError(f"{path} の検証用の open が true ではありません")
            changed[path] = opened
        return changed

    def manifest(self):
        legacy = dict(self.legacy_anchors)
        blocks = {}
        for block in self.blocks.values():
            n = block.chapter.number
            if not block.generate:
                legacy[n] = sorted(set(legacy.get(n, [])) | {old for old in block.anchors if (n, old) not in self.routes})
                continue
            blocks[block.name] = {
                "field": block.field, "chapter": n, "sections": block.sections,
                "anchors": block.anchors, "pages": block.pages,
            }
        materials = {
            name: {"chapter": chapter.number, "path": str(material_path(name).with_suffix(".html")),
                   "anchors": identifiers(segment.nodes)}
            for name, (chapter, segment, _) in self.materials.items()
        }
        course = {str(n): [] for n in range(1, 14)}
        course["5"] = list(REGRESSION)
        course["selected"] = []
        for block in self.blocks.values():
            course["selected" if block.session == "S" else block.session].append(entry(block))
        links = [
            {"block": name, "section": link.section, "label": link.label, "target": link.target}
            for name, link in self.added_links.items()
        ]
        return {"blocks": blocks, "materials": materials, "course": course,
                "added_links": links, "legacy_anchors": legacy}

    def report(self, counts):
        for n, (moved, legacy) in self.coverage.items():
            print(f"dsp{n:02d}  移行の割り当て {moved:4d} 行  旧章のみ {legacy:4d} 行")
        manifest = self.manifest()
        print(f"演習の導入 {sum(chapter.intro is not None for chapter in self.chapters.values())} 章分")
        print(f"生成 {len(manifest['blocks'])} ブロック / {len({b['field'] for b in manifest['blocks'].values()})} 分野")
        print(f"ページ {sum(len(b['pages']) for b in manifest['blocks'].values())}, 仮 ID {len(self.provisional)} 件 "
              f"(旧 ASCII ID を使用 {sum(old == new for _, old, new in self.provisional)} 件)")
        print(f"講義 13 回 + 選択資料 1 件, 必修 32 ブロック, 専用ページ {len(self.materials)} 件 / "
              f"旧 ID {len(self.material_routes)} 件, 追加リンク {len(self.added_links)} 件")
        print("構文木の照合", ", ".join(f"{key}={value}" for key, value in sorted(counts.items())))


def run(args):
    migration = Migration(args.root, layout=args.layout, page_ids=args.page_ids,
                          regression=args.regression, open_all=args.open)
    if args.baseline_layout:
        migration.verify_original_layout(args.baseline_layout, args.baseline_inventory)
    verify_course(args.root, migration.chapters, migration.blocks,
                  opened=None if args.open and not (args.check or args.verify) else args.open)
    visibility = migration.validation_visibility(check=args.check or args.verify)
    outputs = dict(migration.outputs())
    if args.verify and any(path.name == "field.yaml" for path in outputs):
        raise MigrationError("field.yaml がありません "
                             + ", ".join(str(path) for path in outputs if path.name == "field.yaml"))
    counts = Counter()
    for block in migration.blocks.values():
        if block.generate:
            path = block.path()
            text = (args.root / path).read_text(encoding="utf-8") if args.verify else outputs[path]
            counts.update(migration.verify_body(block, text))
    for name in migration.materials:
        path = material_path(name)
        text = (args.root / path).read_text(encoding="utf-8") if args.verify else outputs[path]
        migration.verify_material(name, text)
    if args.check:
        differences = [path for path, text in outputs.items()
                       if not (args.root / path).is_file() or (args.root / path).read_bytes() != text.encode("utf-8")]
        if differences:
            raise MigrationError("生成結果との相違 " + ", ".join(map(str, differences)))
    elif not args.verify:
        generation = outputs | visibility
        if args.open and any(not (args.root / path).resolve().is_relative_to(args.root.resolve())
                             for path in generation):
            raise MigrationError("--open の出力先がビルド用の写しの外を参照しています")
        for path, text in generation.items():
            destination = args.root / path
            destination.parent.mkdir(parents=True, exist_ok=True)
            if path.name == "field.yaml":
                with destination.open("xb") as output:
                    output.write(text.encode("utf-8"))
            elif not destination.exists() or destination.read_bytes() != text.encode("utf-8"):
                destination.write_bytes(text.encode("utf-8"))
    if args.manifest:
        args.manifest.write_text(json.dumps(migration.manifest(), ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    if args.chapter_links:
        migration.write_chapter_links(args.chapter_links)
    if args.site_docs:
        migration.verify_markdown_links(args.site_docs.resolve())
    migration.report(counts)


def main():
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument("command", nargs="?", choices=["generate"], default="generate")
    cli.add_argument("--root", type=Path, default=ROOT)
    cli.add_argument("--layout", type=Path, default=TABLES / "layout.tsv")
    cli.add_argument("--page-ids", type=Path, default=TABLES / "page_ids.tsv")
    cli.add_argument("--baseline-layout", type=Path, help="Compare ranges with layout.txt on the base commit.")
    cli.add_argument("--baseline-inventory", type=Path, help="Include exercise line ranges from the base inv.json.")
    cli.add_argument("--regression", choices=["legacy", "blocks"], default="legacy")
    cli.add_argument("--open", action="store_true", help="Open the course and its blocks in a temporary validation copy.")
    mode = cli.add_mutually_exclusive_group()
    mode.add_argument("--check", action="store_true", help="Fail if the files differ from deterministic generation.")
    mode.add_argument("--verify", action="store_true", help="Verify the Markdown on disk against the old chapters.")
    cli.add_argument("--manifest", type=Path, help="Write the old-to-new HTML verification map outside the checkout.")
    cli.add_argument("--chapter-links", type=Path, help="Write chapter-only link occurrences to TSV.")
    cli.add_argument("--site-docs", type=Path, help="Verify links in the Markdown against a built site.")
    args = cli.parse_args()
    if args.baseline_inventory and not args.baseline_layout:
        cli.error("--baseline-inventory requires --baseline-layout")
    try:
        run(args)
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        print(f"dsp_migration: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
