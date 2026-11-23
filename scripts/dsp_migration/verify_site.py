#!/usr/bin/env python3
"""Hakyll が出力した DSP 旧章と移行ブロックを照合する."""

import argparse
import csv
import re
import subprocess
import sys
from collections import Counter, defaultdict
from dataclasses import dataclass, field
from functools import cache
from html.parser import HTMLParser
from os.path import commonprefix
from pathlib import Path
from urllib.parse import unquote, urljoin, urlsplit, urlunsplit

from course import MATERIALS, REGRESSION, SESSIONS

SCRIPTS = Path(__file__).resolve().parents[1]
TABLES = Path(__file__).resolve().parent
EXERCISE = re.compile(r"exercise-dsp[0-9]+-[0-9]+")
UNCHANGED_CHAPTERS = {1, 5, 12}
LEGACY_TAIL = {2: "本科目で扱う範囲"}
CHAPTER_ROUTE = re.compile(r"/lectures/dsp([0-9]+)\.html")
CSS_VERSION = re.compile(
    rb'(<link rel="stylesheet" type="text/css" href="\.\./style\.css\?v=)\d+(">)'
)
VOID = set("area base br col embed hr img input link meta param source track wbr".split())
HEADINGS = set("h1 h2 h3 h4 h5 h6".split())
BLOCK = HEADINGS | set(
    "blockquote br dd details div dl dt figcaption figure hr li ol p pre section "
    "summary table tbody td th thead tfoot tr ul".split()
)
HTML_SPACE = re.compile(r"[ \t\n\r\f]+")
ADVANCED = re.compile(r" ?\(発展\)")
OPTIONAL = "\x00"
CODE_ANCHOR = re.compile(r"cb[0-9]+(?:-[0-9]+)?")
METRICS = ("pre", "math", "img", "table", "answers")


@dataclass
class Element:
    tag: str
    attrs: dict[str, str | None]
    children: list["Element | str"] = field(default_factory=list)

    def classes(self) -> set[str]:
        return set((self.attrs.get("class") or "").split())


class ArticleParser(HTMLParser):
    def __init__(self):
        super().__init__()
        self.articles = []
        self.stack = []
        self.ids = set()

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if anchor := attrs.get("id") or (attrs.get("name") if tag == "a" else None):
            self.ids.add(anchor)
        if not self.stack and tag != "article":
            return
        element = Element(tag, attrs)
        if self.stack:
            self.stack[-1].children.append(element)
        else:
            self.articles.append(element)
        if tag not in VOID:
            self.stack.append(element)

    def handle_startendtag(self, tag, attrs):
        self.handle_starttag(tag, attrs)
        if tag not in VOID:
            self.handle_endtag(tag)

    def handle_endtag(self, tag):
        for index in range(len(self.stack) - 1, -1, -1):
            if self.stack[index].tag == tag:
                del self.stack[index:]
                break

    def handle_data(self, data):
        if self.stack:
            self.stack[-1].children.append(data)


def read_html(path):
    parser = ArticleParser()
    parser.feed(path.read_text(encoding="utf-8"))
    parser.close()
    return parser


@cache
def article_body(path, *, block_page=False):
    parser = read_html(path)
    if len(parser.articles) != 1 or parser.stack:
        raise ValueError(f"{path} の article を特定できません.")
    article, = parser.articles
    if "placeholder" in article.classes():
        raise ValueError(f"{path} に本文が出力されていません.")
    return [
        node for node in article.children
        if not (
            isinstance(node, Element)
            and (
                (node.tag == "header" and "article-header" in node.classes())
                or (node.tag == "nav" and "chapter-nav-card" in node.classes())
                or (block_page and node.tag == "nav" and "block-prerequisites" in node.classes())
            )
        )
    ]


def sections(body):
    """article の直下にある見出しで本文を区切る."""
    chunks = [(None, [])]
    for node in body:
        if isinstance(node, Element) and node.tag in HEADINGS and node.attrs.get("id"):
            chunks.append((node.attrs["id"], []))
        chunks[-1][1].append(node)
    return chunks


def elements(nodes):
    for node in nodes:
        if isinstance(node, Element):
            yield node
            yield from elements(node.children)


def stable_anchors(nodes):
    ids = Counter()
    for node in elements(nodes):
        anchor = node.attrs.get("id")
        if anchor and not (
            CODE_ANCHOR.fullmatch(anchor)
            or (anchor.startswith("ref-") and "csl-entry" in node.classes())
        ):
            ids[anchor] += 1
        if node.tag == "a" and (name := node.attrs.get("name")) and name != anchor:
            ids[name] += 1
    return ids


def collapse_space(text):
    return HTML_SPACE.sub(" ", text).strip(" ")


def rendered_text(nodes, allow_advanced_removal=False):
    parts = []
    for node in nodes:
        if isinstance(node, str):
            parts.append(node)
            continue
        if node.tag in {"script", "style"}:
            continue
        label = node.tag in HEADINGS or node.tag == "a"
        value = rendered_text(node.children, allow_advanced_removal and not label)
        if allow_advanced_removal and label:
            # The marker preserves whether the old label had a space before 発展.
            value = ADVANCED.sub(lambda match: OPTIONAL + match[0] + OPTIONAL,
                                 HTML_SPACE.sub(" ", value))
        if node.tag in BLOCK:
            value = " " + value + " "
        parts.append(value)
    return "".join(parts)


def text_difference(old, new, *, allow_advanced_removal=True):
    expected = collapse_space(rendered_text(old, allow_advanced_removal=allow_advanced_removal))
    actual = collapse_space(rendered_text(new))
    pieces = expected.split(OPTIONAL)
    if len(pieces) == 1:
        if expected == actual:
            return None
    else:
        pattern = "".join(
            "(?:" + re.escape(piece) + ")?" if index % 2 else re.escape(piece)
            for index, piece in enumerate(pieces)
        )
        if re.fullmatch(pattern, actual):
            return None
        # Align optional labels before locating the first mismatching mandatory text.
        offset = 0
        aligned = []
        for index, piece in enumerate(pieces):
            if index % 2 and not actual.startswith(piece, offset):
                continue
            aligned.append(piece)
            offset += len(piece)
        expected = "".join(aligned)
    prefix = len(commonprefix((expected, actual)))
    start = max(0, prefix - 25)
    return f"旧 {expected[start:prefix + 48]!r} / 新 {actual[start:prefix + 48]!r}"


def content_metrics(nodes):
    counts = Counter({key: 0 for key in METRICS})
    for node in elements(nodes):
        if node.tag in {"pre", "img", "table"}:
            counts[node.tag] += 1
        if node.tag == "span" and "math" in node.classes():
            counts["math"] += 1
        if node.tag == "details" and "protected" in node.classes() and node.attrs.get("data-pass"):
            counts["answers"] += 1
    return counts


def code_blocks(nodes):
    return [rendered_text(node.children) for node in elements(nodes) if node.tag == "pre"]


def shape(node):
    """Check structure and attributes of rendered tables and protected answers."""
    attrs = tuple(sorted((key, val) for key, val in node.attrs.items()
                         if not (node.tag == "a" and key == "href")
                         and not (node.tag == "img" and key == "src")
                         and not (key == "id" and val and CODE_ANCHOR.fullmatch(val))))
    return node.tag, attrs, tuple(shape(child) for child in node.children if isinstance(child, Element))


def compare_content(old, new, old_route, new_route, label, issues, *, allow_advanced_removal=True):
    old_metrics, new_metrics = content_metrics(old), content_metrics(new)
    if old_metrics != new_metrics:
        issues.append(f"{label} の件数が異なります. 旧 {dict(old_metrics)} / 新 {dict(new_metrics)}")
    if code_blocks(old) != code_blocks(new):
        issues.append(f"{label} の整形済みコードが異なります.")
    old_elements, new_elements = list(elements(old)), list(elements(new))

    def images(items, route):
        result = []
        for node in items:
            if node.tag == "img":
                attrs = dict(node.attrs)
                if attrs.get("src"):
                    attrs["src"] = urljoin("/" + str(route), attrs["src"])
                result.append(attrs)
        return result

    if images(old_elements, old_route) != images(new_elements, new_route):
        issues.append(f"{label} の画像の参照先または属性が異なります.")
    for tag, name in (("table", "表"), ("details", "回答や補足の箱"), ("span", "数式")):
        def selected(items):
            return [node for node in items if node.tag == tag and (tag != "span" or "math" in node.classes())]

        before, after = selected(old_elements), selected(new_elements)
        if len(before) != len(after) or any(
            shape(left) != shape(right)
            or text_difference(left.children, right.children, allow_advanced_removal=allow_advanced_removal)
            for left, right in zip(before, after)
        ):
            issues.append(f"{label} の{name}の中身が異なります.")
    if difference := text_difference(old, new, allow_advanced_removal=allow_advanced_removal):
        issues.append(f"{label} の本文が異なります. {difference}")
    return new_metrics


def compare_legacy(base, opened, issues):
    same = 0
    for chapter in range(1, 17):
        relative = Path("lectures") / f"dsp{chapter}.html"
        try:
            before = (base / relative).read_bytes()
            after = (opened / relative).read_bytes()
        except OSError as exc:
            issues.append(str(exc))
            continue
        before = CSS_VERSION.sub(rb"\g<1>VERSION\2", before)
        after = CSS_VERSION.sub(rb"\g<1>VERSION\2", after)
        if before != after:
            line = commonprefix((before, after)).count(b"\n") + 1
            issues.append(f"{relative} の HTML が {line} 行目から異なります.")
            continue
        same += 1
    return same


@dataclass(frozen=True)
class Section:
    old_id: str | None
    new_id: str | None
    body: list


def read_rows(path):
    with path.open(encoding="utf-8", newline="") as source:
        return list(csv.DictReader(source, delimiter="\t"))


def regression_links(base, table):
    """Locate the table's regression anchors in the original build."""
    pages_by_block = {}
    result = {}
    for row in read_rows(table):
        block, anchor = row["block"], row["block_id"]
        folder = Path("fields", "regression", block)
        if block not in pages_by_block:
            pages_by_block[block] = {
                page.relative_to(base).as_posix(): read_html(page).ids
                for page in sorted((base / folder).glob("*.html"))
            }
        pages = pages_by_block[block]
        matches = [page for page, ids in pages.items() if anchor in ids] if anchor else [
            (folder / "index.html").as_posix()
        ]
        if len(matches) != 1 or matches[0] not in pages:
            raise ValueError(f"回帰の対応先が旧ビルドに一意にありません ({block}#{anchor}).")
        result[row["old_id"]] = ("/" + matches[0], anchor)
    return result


def absolute_href(route, href):
    parts = urlsplit(urljoin("/" + route.as_posix(), href))
    if parts.scheme or parts.netloc:
        return parts.geturl()
    return parts._replace(fragment=unquote(parts.fragment)).geturl()


def body_hrefs(nodes, skip=None):
    for node in elements(nodes):
        if node is skip:
            continue
        href = node.attrs.get("href")
        if href is None:
            continue
        # Pandoc numbers highlighted code lines separately on each page.
        if (node.tag == "a" and node.attrs.get("aria-hidden") == "true"
                and node.attrs.get("tabindex") == "-1" and re.fullmatch(r"#cb[0-9]+-[0-9]+", href)):
            continue
        yield href


class LinkTargets:
    def __init__(self, expected, regression=None, materials=None, added=()):
        self.regression = regression or {}
        self.materials = {}
        for name, (chapter, body) in (materials or {}).items():
            for anchor in stable_anchors(body):
                self.materials[chapter, anchor] = name
        self.added = {(row["block"], row["section"]): row for row in added}
        self.moved = defaultdict(list)
        self.block_routes = {}
        self.block_anchors = {}
        for chapter, field, name, pages in expected:
            for page, sections_on_page in pages.items():
                path = f"/fields/{field}/{name}/{page}.html"
                if page == "index":
                    self.block_routes[name] = path
                for section in sections_on_page:
                    for anchor in stable_anchors(section.body):
                        destination = "exercises" if anchor == "演習" else anchor
                        self.block_anchors[name, anchor] = f"{path}#{destination}"
                        self.moved[chapter, anchor].append((name, path, destination))

    def href(self, route, current_block, href, *, material=False):
        absolute = absolute_href(route, href)
        parts = urlsplit(absolute)
        chapter_match = CHAPTER_ROUTE.fullmatch(parts.path)
        if parts.scheme or parts.netloc or not chapter_match or (material and parts.query):
            return absolute
        chapter, old_id = int(chapter_match[1]), parts.fragment
        if material:
            dedicated = self.materials.get((chapter, old_id)) if old_id else {1: "guide", 12: "capstone"}.get(chapter)
            if dedicated:
                return urlunsplit(("", "", f"/courses/dsp/{dedicated}.html", "", old_id))
        if not old_id:
            return absolute
        if chapter == 5 and old_id in self.regression:
            path, anchor = self.regression[old_id]
        elif choices := self.moved.get((chapter, old_id)):
            _, path, anchor = next((choice for choice in choices if choice[0] == current_block), choices[0])
        else:
            return absolute
        return urlunsplit(("", "", path, parts.query, anchor))

    def added_link(self, old, new, route, row):
        context = row["context"].replace("**", "")
        label = row["label"]
        paragraphs = [collapse_space(rendered_text(node.children)) for node in elements(old) if node.tag == "p"]
        matches = [text for text in paragraphs if text.count(context) == 1]
        if len(matches) != 1:
            raise ValueError(f"{route} の旧本文から {label!r} の位置を特定できません.")
        before = matches[0].split(context, 1)[0] + context.split(label, 1)[0]
        name, _, anchor = row["target"].removeprefix("block:").partition("#")
        url = self.block_anchors[name, anchor] if anchor else self.block_routes[name]

        def mark(nodes, candidate):
            for node in nodes:
                if node is candidate:
                    yield "\x00" + rendered_text(node.children)
                elif isinstance(node, Element):
                    yield Element(node.tag, node.attrs, list(mark(node.children, candidate)))
                else:
                    yield node

        found = []
        for node in elements(new):
            if node.tag != "p" or collapse_space(rendered_text(node.children)) != matches[0]:
                continue
            for child in elements(node.children):
                if (child.tag == "a" and child.attrs.get("href")
                        and absolute_href(route, child.attrs["href"]) == url
                        and collapse_space(rendered_text(child.children)) == label):
                    marked = collapse_space(rendered_text(list(mark(node.children, child))))
                    if marked.split("\x00", 1)[0] == before:
                        found.append(child)
        if len(found) != 1:
            raise ValueError(f"{route} の追加リンク {label!r} の位置, 文字列または行き先が異なります.")
        return found[0]

    def compare(self, old, new, old_route, new_route, name, label, issues, *, section=None, material=False):
        skip = None
        if row := self.added.get((name, section)):
            try:
                skip = self.added_link(old, new, new_route, row)
            except ValueError as error:
                issues.append(str(error))
        want = [self.href(old_route, name, href, material=material) for href in body_hrefs(old)]
        got = [absolute_href(new_route, href) for href in body_hrefs(new, skip)]
        if len(want) != len(got):
            issues.append(f"{label} の href の件数が異なります. 旧 {len(want)} / 新 {len(got)}.")
        for i, (expected, actual) in enumerate(zip(want, got), 1):
            if expected != actual:
                issues.append(f"{label} の href {i} 件目が異なります. 期待値 {expected!r} / 実際 {actual!r}.")
        return len(want) + int(skip is not None)


def expected_materials(base):
    materials = {}
    for name, (chapter, start, _) in MATERIALS.items():
        body = article_body(base / "lectures" / f"dsp{chapter}.html")
        if start != "@start":
            positions = [i for i, node in enumerate(body)
                         if isinstance(node, Element) and node.tag in HEADINGS and node.attrs.get("id") == start]
            if len(positions) != 1:
                raise ValueError(f"dsp{chapter}.html#{start} の専用ページの範囲を特定できません.")
            body = body[positions[0]:]
        materials[name] = (chapter, body)
    return materials


def compare_material(opened, name, chapter, old, links, issues):
    route = Path("courses", "dsp", f"{name}.html")
    new = article_body(opened / route)
    old_route = Path("lectures", f"dsp{chapter}.html")
    old_ids = stable_anchors(old)
    if old_ids != stable_anchors(new):
        issues.append(f"{route} の旧 ID が一致しません.")

    def headings(body):
        return [(node.tag, node.attrs, collapse_space(rendered_text(node.children)))
                for node in elements(body) if node.tag in HEADINGS]

    if headings(old) != headings(new):
        issues.append(f"{route} の旧見出しの深さ, 属性または文字列が一致しません.")
    compare_content(old, new, old_route, route, str(route), issues, allow_advanced_removal=False)
    hrefs = links.compare(old, new, old_route, route, name, str(route), issues, material=True)
    return len(old_ids), hrefs


def compare_course(base, opened, layout, links, issues):
    directory = Path("courses", "dsp")
    routes = {str(n) for n in range(1, 14)} | {"index", "selected", *MATERIALS}
    if {p.stem for p in (opened / directory).glob("*.html")} != routes:
        issues.append(f"{directory} のページ集合が一致しません.")

    def in_groups(body, group, tag):
        return [child for node in elements(body) if group in node.classes()
                for child in node.children if isinstance(child, Element) and child.tag == tag]

    def named_links(items, route):
        return [(absolute_href(route, node.attrs["href"]), collapse_space(rendered_text(node.children)))
                for node in elements(items) if node.tag == "a" and node.attrs.get("href")]

    index = directory / "index.html"
    body = article_body(opened / index)
    want_sessions = [(f"/{directory}/{n}.html", title) for n, (title, _) in enumerate(SESSIONS, 1)]
    if named_links(in_groups(body, "course-sessions", "li"), index) != want_sessions:
        issues.append(f"{index} の 13 回の題名または並びが異なります.")
    if named_links(in_groups(body, "course-supplements", "h2"), index) != [(f"/{directory}/selected.html", "選択資料")]:
        issues.append(f"{index} の選択資料が異なります.")

    def check_material_links(body, route, material):
        want = [(f"/{directory}/{material}.html", MATERIALS[material][2])] if material else []
        if named_links(in_groups(body, "course-materials", "li"), route) != want:
            issues.append(f"{route} の専用ページへのリンクが異なります.")

    check_material_links(body, index, None)
    expected = defaultdict(list, {"5": list(REGRESSION)})
    for row in read_rows(layout):
        chapter = int(row["chapter"])
        old_route = base / "lectures" / f"dsp{chapter}.html"
        old_sections = dict(sections(article_body(old_route)))
        exercises = []
        for ident in filter(None, row["exercises"].split(",")):
            old_text = collapse_space(rendered_text(old_sections[ident]))
            names = re.findall(r"提出ファイル(?:名:|は) +([^ ]+?\.py)(?= |$)", old_text)
            if len(names) > 1:
                raise ValueError(f"{old_route}#{ident} の提出ファイル名が複数あります.")
            exercises.append((ident, names[0] if names else None))
        section = "selected" if row["session"] == "S" else row["session"]
        expected[section].append((row["block"], row["level"], exercises))

    required = submitted = 0
    for n, (_, material) in [*enumerate(SESSIONS, 1), ("selected", ("選択資料", None))]:
        route = directory / f"{n}.html"
        body = article_body(opened / route)
        check_material_links(body, route, material)
        if n == 5:
            paragraphs = [node for node in body if isinstance(node, Element) and node.tag == "p"]
            if len(paragraphs) != 3 or any(node.tag == "br" for node in elements(paragraphs)):
                issues.append(f"{route} の intro は強制改行のない 3 段落である必要があります.")
        rows = in_groups(body, "session-blocks", "li")
        entries = expected[str(n)]
        if len(rows) != len(entries):
            issues.append(f"{route} のブロック数が異なります.")
        for item, (block, level, exercises) in zip(rows, entries):
            block_route = links.block_routes.get(block, f"/fields/regression/{block}/index.html")
            first = next((node for node in item.children if isinstance(node, Element) and node.tag == "a"), None)
            want_label = "必修" if level == "required" else "発展"
            text = collapse_space("".join(node for node in item.children if isinstance(node, str)))
            if first is None or absolute_href(route, first.attrs["href"]) != block_route or not text.startswith(f"({want_label} / "):
                issues.append(f"{route} の {block} の並びまたは必修・発展が異なります.")
            required += text.startswith("(必修 / ")
            items = in_groups(item.children, "session-exercises", "li")
            actual = []
            for exercise in items:
                anchors = named_links([exercise], route)
                names = [rendered_text(node.children) for node in elements(exercise.children) if node.tag == "code"]
                submitted += len(names)
                actual.append(([href for href, _ in anchors], names))
            want = [([block_route.removesuffix("index.html") + f"exercises.html#{ident}"],
                     [filename] if filename else []) for ident, filename in exercises]
            if actual != want:
                issues.append(f"{route} の {block} の演習または提出ファイルが異なります.")
    if required != 32:
        issues.append(f"{directory} の必修ブロック数は {required} 件です (期待値 32).")
    return required, submitted


def expected_blocks(base, layout, page_table):
    """Derive sections and routes directly from the original Hakyll HTML."""
    declared_ids = {(int(row["chapter"].removeprefix("dsp")), row["old_id"]): row["page_id"]
                    for row in read_rows(page_table)}
    chapter_sections = {
        number: sections(article_body(base / "lectures" / f"dsp{number}.html"))
        for number in range(1, 17)
    }
    indexes = {
        number: {anchor: i for i, (anchor, _) in enumerate(chunks)}
        for number, chunks in chapter_sections.items()
    }
    covered = defaultdict(Counter)
    built = []

    for row in read_rows(layout):
        chapter, name = int(row["chapter"]), row["block"]
        chunks = chapter_sections[chapter]
        index = indexes[chapter]
        ranges = []
        for spec in row["ranges"].split(","):
            start, end = spec.split("..")
            begin_index = 0 if start == "@start" else index[start]
            end_index = len(chunks) if end == "@end" else index[end]
            if begin_index >= end_index:
                raise ValueError(f"dsp{chapter} の範囲が不正です ({name}, {spec}).")
            ranges.append(chunks[begin_index:end_index])

        exercise_order = [anchor for anchor, _ in chunks if anchor and EXERCISE.fullmatch(anchor)]
        exercise_ids = sorted(filter(None, row["exercises"].split(",")), key=index.__getitem__)
        exercise_chunks = []
        if exercise_ids:
            intro = index["演習"]
            exercise_chunks.extend(chunks[intro:index[exercise_order[0]]])
            for exercise in exercise_ids:
                next_exercise = exercise_order.index(exercise) + 1
                end_index = index[exercise_order[next_exercise]] if next_exercise < len(exercise_order) else len(chunks)
                exercise_chunks.extend(chunks[index[exercise]:end_index])

        for anchor, _ in [item for span in ranges for item in span] + exercise_chunks:
            covered[chapter][anchor] += 1
        if row["generate"] == "false":
            continue

        def depth(chunk):
            anchor, nodes = chunk
            return int(nodes[0].tag[1:]) - 1 if anchor is not None else 0

        shift = int(sum(depth(chunk) == 1 for span in ranges for chunk in span) <= 1)
        pages = {"index": []}
        current = "index"
        if row["copy"]:
            if (chapter, name, row["copy"]) != (10, "dl-training-procedure", "opening-warn"):
                raise ValueError(f"{name} の複製指定が不正です.")
            _, lead = chunks[0]
            first = next((node for node in lead if isinstance(node, Element)), None)
            if first is None or first.tag != "div" or "warn" not in first.classes():
                raise ValueError("dsp10.html の冒頭の warn を特定できません.")
            pages["index"].append(Section(None, None, [first]))

        first_content = True
        for span in ranges:
            for i, chunk in enumerate(span):
                anchor, old_nodes = chunk
                new_anchor = anchor
                if anchor is None:
                    if not any(isinstance(node, Element) or node.strip() for node in old_nodes):
                        continue
                elif not first_content and (depth(chunk) <= shift + 1 or (i == 0 and depth(chunk) >= 3)):
                    new_anchor = declared_ids[(chapter, anchor)]
                    if new_anchor in pages or new_anchor == "exercises":
                        raise ValueError(f"{name} のページ ID が重複しています ({new_anchor}).")
                    current = new_anchor
                    pages[current] = []
                first_content = False
                pages[current].append(Section(anchor, new_anchor, old_nodes))
        if exercise_chunks:
            pages["exercises"] = [Section(anchor, "exercises" if anchor == "演習" else anchor, body)
                                  for anchor, body in exercise_chunks]
        built.append((chapter, row["field"], name, pages))

    # Every old HTML section has one owner. The shared exercise introduction is
    # copied to each exercises page in its chapter. The special warn copy is above.
    for chapter, chunks in chapter_sections.items():
        if chapter in UNCHANGED_CHAPTERS:
            if covered[chapter]:
                raise ValueError(f"dsp{chapter} は旧章に残します.")
            continue
        tail = indexes[chapter].get(LEGACY_TAIL.get(chapter, ""), len(chunks))
        for position, (anchor, _) in enumerate(chunks):
            count = covered[chapter][anchor]
            expected = 0 if position >= tail else 1
            if (anchor == "演習" and expected == 1 and count >= 1) or count == expected:
                continue
            raise ValueError(f"dsp{chapter}.html#{anchor} の割り当てが {count} 件です (期待値 {expected}).")
    return built


def compare_block(opened, chapter, field, name, pages, issues, routes, links):
    directory = opened / "fields" / field / name
    old_route = Path("lectures", f"dsp{chapter}.html")
    actual_pages = {page.stem for page in directory.glob("*.html")}
    if actual_pages != pages.keys():
        issues.append(f"{name} のページ集合が異なります. 差分 {sorted(actual_pages ^ pages.keys())}.")
    counts = Counter({key: 0 for key in METRICS})
    checked_sections = 0
    checked_hrefs = 0
    for key, wanted in pages.items():
        path = directory / f"{key}.html"
        route = path.relative_to(opened)
        try:
            actual = sections(article_body(path, block_page=True))
        except (OSError, ValueError) as exc:
            issues.append(str(exc))
            continue
        if not wanted or wanted[0].new_id is not None:
            _, preamble = actual.pop(0)
            if any(isinstance(node, Element) or node.strip() for node in preamble):
                issues.append(f"{route} の最初の見出しより前に対応する旧本文がありません.")
        if [head for head, _ in actual] != [item.new_id for item in wanted]:
            issues.append(f"{route} の見出し ID または順序が割り当てと一致しません.")
            continue

        for (head, new_nodes), item in zip(actual, wanted, strict=True):
            label = f"{route}#{head}" if head else str(route)
            old_ids = stable_anchors(item.body)
            want_ids, got_ids = old_ids.copy(), stable_anchors(new_nodes)
            if item.new_id != item.old_id:
                want_ids[item.new_id] += 1
            for anchor in sorted(want_ids.keys() | got_ids.keys()):
                if want_ids[anchor] != got_ids[anchor]:
                    issues.append(f"dsp{chapter}#{anchor} → {label} の ID が {got_ids[anchor]} 件です "
                                  f"(期待値 {want_ids[anchor]}).")
            for anchor in old_ids:
                destination = item.new_id if anchor == item.old_id else anchor
                if got_ids[anchor] == want_ids[anchor] and got_ids[destination] == want_ids[destination]:
                    routes[(chapter, anchor)].append((name, key, destination))

            counts.update(compare_content(item.body, new_nodes, old_route, route, label, issues))
            checked_hrefs += links.compare(item.body, new_nodes, old_route, route, name, label, issues,
                                           section=item.old_id)
            checked_sections += 1
    return checked_sections, checked_hrefs, counts


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("base_docs", type=Path)
    parser.add_argument("open_docs", type=Path)
    parser.add_argument("--layout", type=Path, default=TABLES / "layout.tsv")
    parser.add_argument("--page-ids", type=Path, default=TABLES / "page_ids.tsv")
    parser.add_argument("--regression", choices=("legacy", "blocks"), default="legacy")
    parser.add_argument("--reg-anchors", type=Path, default=SCRIPTS / "reg_old_anchors.tsv")
    args = parser.parse_args()
    issues = []
    legacy_count = compare_legacy(args.base_docs, args.open_docs, issues)
    try:
        expected = expected_blocks(args.base_docs, args.layout, args.page_ids)
        materials = expected_materials(args.base_docs)
        regression = regression_links(args.base_docs, args.reg_anchors) if args.regression == "blocks" else None
        links = LinkTargets(expected, regression, materials, read_rows(TABLES / "added_links.tsv"))
    except (OSError, ValueError, KeyError) as exc:
        print(f"旧章から期待値を作成できません. {exc}", file=sys.stderr)
        return 1

    counts = Counter({key: 0 for key in METRICS})
    sections_checked = 0
    hrefs_checked = 0
    routes = defaultdict(list)
    for chapter, field, name, pages in expected:
        section_count, href_count, block_counts = compare_block(
            args.open_docs, chapter, field, name, pages, issues, routes, links,
        )
        sections_checked += section_count
        hrefs_checked += href_count
        counts.update(block_counts)

    material_ids = material_hrefs = required = submitted = 0
    try:
        for name, (chapter, body) in materials.items():
            ids, hrefs = compare_material(args.open_docs, name, chapter, body, links, issues)
            material_ids += ids
            material_hrefs += hrefs
        required, submitted = compare_course(args.base_docs, args.open_docs, args.layout, links, issues)
    except (OSError, ValueError, KeyError) as exc:
        issues.append(str(exc))

    if subprocess.run(
        [sys.executable, "-B", str(SCRIPTS / "check_local_refs.py"), str(args.open_docs)],
        check=False,
    ).returncode:
        issues.append("サイト内参照の検査が失敗しました.")
    for issue in issues:
        print(issue, file=sys.stderr)
    print(
        f"HTML 照合結果 旧章 {legacy_count}/16, ブロック {len(expected)}, "
        f"節 {sections_checked}, href {hrefs_checked} 件, "
        f"旧章と旧 ID の組 {len(routes)} 件, 確認した移行先 {sum(map(len, routes.values()))} 件, "
        + ", ".join(f"{name} {count}" for name, count in counts.items())
        + f". 専用ページ {len(materials)} 件 / 旧 ID {material_ids} 件 / href {material_hrefs} 件, "
        f"講義の必修 {required} 件 / 提出ファイル名 {submitted} 件"
        + f". 問題 {len(issues)} 件."
    )
    return int(bool(issues))


if __name__ == "__main__":
    sys.exit(main())
