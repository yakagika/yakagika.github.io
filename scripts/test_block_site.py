#!/usr/bin/env python3
"""Run the block-site acceptance checks after stack build.

Usage: python3 scripts/test_block_site.py
"""

from __future__ import annotations

import os
import re
import shutil
import subprocess
import sys
import tempfile
from collections import Counter
from html import escape, unescape
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urljoin, urlsplit
from xml.etree import ElementTree

REPO = Path(__file__).resolve().parent.parent
FIXTURE_ENV = {**os.environ, "BLOG_FIXTURES": "1"}

REGRESSION = "fields/sample-regression"
INFERENCE = "fields/sample-inference"
PRACTICE = "courses/sample-practice"
STATISTICS = "courses/sample-statistics"
DRAFT = "courses/sample-draft"
DRAFT_GUIDE = f"{DRAFT}/guide.html"
DRAFT_ROUTES = {f"{DRAFT}/index.html", f"{DRAFT}/1.html", f"{DRAFT}/follow-up.html", DRAFT_GUIDE}
OVERVIEW = f"{REGRESSION}/sample-overview/index.html"
VARIABLES = f"{REGRESSION}/sample-overview/variables.html"
FITTING = f"{REGRESSION}/sample-fitting/index.html"
FIT = f"{REGRESSION}/sample-fitting/fit.html"
RESIDUALS = f"{REGRESSION}/sample-fitting/residuals.html"
EXERCISES = f"{REGRESSION}/sample-fitting/exercises.html"
PREDICTION = f"{REGRESSION}/sample-prediction/index.html"
EXTENSION = f"{REGRESSION}/sample-extension/index.html"
CHECK_SAMPLE = f"{REGRESSION}/sample-extension/check-sample.html"
SAMPLING = f"{INFERENCE}/sample-sampling/index.html"
SAMPLE = f"{INFERENCE}/sample-sampling/sample.html"
UNCERTAINTY = f"{INFERENCE}/sample-uncertainty/index.html"
PRACTICE_1 = f"{PRACTICE}/1.html"
PRACTICE_2 = f"{PRACTICE}/2.html"
FOLLOW_UP = f"{PRACTICE}/follow-up.html"
GUIDE = f"{PRACTICE}/guide.html"
CHECKLIST = f"{PRACTICE}/checklist.html"
PRACTICE_INDEX = f"{PRACTICE}/index.html"
PRACTICE_CONTENT = {
    PRACTICE_INDEX: ('class="course-sessions"', 'class="course-materials"', 'class="course-supplements"'),
    PRACTICE_1: ("FIXTURE_OPEN_FIRST_INTRO",),
    PRACTICE_2: ("FIXTURE_OPEN_SECOND_INTRO",),
    FOLLOW_UP: ("FIXTURE_OPEN_SUPPLEMENT_INTRO",),
    GUIDE: ("FIXTURE_OPEN_GUIDE_BODY", "FIXTURE_OPEN_GUIDE_ANSWER"),
    CHECKLIST: ("FIXTURE_OPEN_CHECKLIST_BODY",),
}
STATISTICS_1 = f"{STATISTICS}/1.html"
STATISTICS_2 = f"{STATISTICS}/2.html"

OPEN_ROUTES = {
    f"{REGRESSION}/index.html",
    f"{INFERENCE}/index.html",
    OVERVIEW, VARIABLES, FITTING, FIT, RESIDUALS, EXERCISES, EXTENSION, CHECK_SAMPLE,
    SAMPLING, SAMPLE, UNCERTAINTY, f"{INFERENCE}/sample-uncertainty/variation.html",
    f"{PRACTICE}/index.html", PRACTICE_1, PRACTICE_2, FOLLOW_UP, GUIDE, CHECKLIST,
    f"{STATISTICS}/index.html", STATISTICS_1, STATISTICS_2,
}
CLOSED_ROUTES = {
    PREDICTION,
    f"{REGRESSION}/sample-prediction/forecast.html",
    f"{REGRESSION}/sample-prediction/exercises.html",
} | DRAFT_ROUTES


def check(condition: bool, message: str) -> None:
    if not condition:
        raise AssertionError(message)


class Page(HTMLParser):
    """Collect IDs, headings and local links, including links grouped by navigation element."""

    def __init__(self, site: Path, path: str) -> None:
        super().__init__()
        self.path = path
        self.source = (site / path).read_text(encoding="utf-8")
        self.ids: Counter[str] = Counter()
        self.headings: list[tuple[str, str | None]] = []
        self.toc_ancestors: list[tuple[str, str | None]] = []
        self.toc_targets: list[tuple[str, str, int]] = []
        self.robots: set[str] = set()
        self.links: set[str] = set()
        self.prerequisites: list[str] = []
        self.occurrences: list[dict[str, str]] = []
        self.nav_stack: list[list[str]] = []
        self.items: list[dict[str, str] | None] = []
        self.session_depth: int | None = None
        self.session_rows: list[tuple[set[str], list[str]]] = []
        self.feed(self.source)
        self.close()

    def handle_starttag(self, tag: str, attributes: list[tuple[str, str | None]]) -> None:
        attrs = dict(attributes)
        classes = (attrs.get("class") or "").split()
        anchor = attrs.get("id")
        if anchor:
            self.ids[anchor] += 1
        if tag in {"article", "section"}:
            self.toc_ancestors.append((tag, (anchor or "") if "id" in attrs else None))
        if (tag in {"h2", "h3", "h4"} and not self.items
                and any(kind == "article" for kind, _ in self.toc_ancestors)):
            # Match the TOC script's closest('section[id]') fallback.
            target = anchor or next((ident for kind, ident in reversed(self.toc_ancestors)
                                     if kind == "section" and ident is not None), "")
            self.toc_targets.append((tag, target, self.getpos()[0]))
        if tag in {"h1", "h2", "h3", "h4", "h5", "h6"}:
            self.headings.append((tag, anchor))
        if tag == "meta" and attrs.get("name") == "robots":
            self.robots.update(re.split(r"[,\s]+", attrs.get("content") or ""))
        if tag == "nav":
            self.nav_stack.append(classes)
        if tag == "ol" and "session-blocks" in classes:
            self.session_depth = len(self.items)
        if tag == "li":
            if self.session_depth == len(self.items):
                self.session_rows.append((set(), []))
            occurrence = {} if "course-occurrence" in classes else None
            self.items.append(occurrence)
            if occurrence is not None:
                self.occurrences.append(occurrence)
        href = attrs.get("href")
        if tag == "a" and href:
            url = urlsplit(urljoin(f"/{self.path}", href))
            if url.scheme or url.netloc:
                return
            target = unquote(url.path).lstrip("/")
            if url.fragment:
                target += "#" + unquote(url.fragment)
            self.links.add(target)
            if any("block-prerequisites" in names for names in self.nav_stack):
                self.prerequisites.append(target)
            rel = attrs.get("rel")
            if self.items and self.items[-1] is not None:
                self.items[-1][rel or target] = target
            if self.session_depth is not None and len(self.items) > self.session_depth:
                self.session_rows[-1][0].add(target)

    def handle_endtag(self, tag: str) -> None:
        if tag in {"article", "section"}:
            for index in range(len(self.toc_ancestors) - 1, -1, -1):
                if self.toc_ancestors[index][0] == tag:
                    del self.toc_ancestors[index:]
                    break
        if tag == "nav" and self.nav_stack:
            self.nav_stack.pop()
        if tag == "li" and self.items:
            self.items.pop()
        if tag == "ol" and self.session_depth == len(self.items):
            self.session_depth = None

    def handle_data(self, text: str) -> None:
        if self.session_depth is not None and len(self.items) > self.session_depth:
            self.session_rows[-1][1].append(text)

    def has_links(self, *targets: str) -> None:
        for target in targets:
            check(target in self.links, f"{self.path}: missing link to {target}")

    def check_toc_targets(self) -> None:
        for tag, target, line in self.toc_targets:
            check(bool(target.strip()), f"{self.path}:{line}: {tag} in the table of contents has no ID")
        duplicates = sorted(ident for ident, count in self.ids.items() if count > 1)
        check(not duplicates, f"{self.path}: duplicate IDs: {duplicates}")


def local_targets(path: str, html: str) -> list[str]:
    return [
        unquote(urlsplit(urljoin(f"/{path}", unescape(href))).path).lstrip("/")
        for href in re.findall(r' href="([^"]+)"', html)
    ]


def check_reference_material_names(site: Path) -> None:
    lectures = (site / "lectures.html").read_text(encoding="utf-8")
    description = ("各講義で共通の内容を扱うため, 共通の資料として分離しました. "
                   "各講義ではそれぞれ必要な部分を利用します.")
    check("教科書" not in lectures, "Lecture index still says 教科書")
    check(f">{description}</p>" in lectures, "Lecture index lacks the specified description")
    check('id="参考資料-分野別">参考資料 (分野別)</h2>' in lectures, "Wrong field heading")
    check(lectures.count('class="series-cta">参考資料を見る →</span>') == 8, "Wrong field card labels")
    for path in (site / "fields").glob("*/index.html"):
        check("教科書" not in path.read_text(encoding="utf-8"), f"{path}: still says 教科書")


def check_reference_navigation(site: Path) -> None:
    for index in sorted((site / "fields").glob("*/index.html")):
        field = index.relative_to(site).as_posix()
        field_index = Page(site, field)
        listing = field_index.source.split('<ol class="field-blocks">', 1)[1].split("</article>", 1)[0]
        blocks: dict[str, list[str]] = {}
        for route in local_targets(field, listing):
            name = route.rsplit("/", 1)[0]
            blocks.setdefault(name, [])
            if route not in blocks[name]:
                blocks[name].append(route)
        block_indexes = [routes[0] for routes in blocks.values()]
        block_pages = {route: Page(site, route) for routes in blocks.values() for route in routes}
        check(set(block_pages) == {path.relative_to(site).as_posix() for path in index.parent.glob("*/*.html")},
              f"{field}: field listing does not cover all block pages")
        public = [route for route, page in block_pages.items() if "noindex" not in page.robots]
        titles = {route: re.search(r"<title>yakagika - (.*?)</title>", page.source)[1]
                  for route, page in {field: field_index, **block_pages}.items()}
        previous: dict[str, str] = {}
        following: dict[str, str] = {}
        for route, page in {field: field_index, **block_pages}.items():
            sidebar = re.search(r'<div id="lecture-toc" data-mode="series">(.*?)</nav>', page.source, re.S)
            check(sidebar is not None, f"{route}: missing series Index")
            sidebar_links = local_targets(route, sidebar[1])
            block_routes = blocks.get(route.rsplit("/", 1)[0], [])
            expected_links = [field, *block_indexes]
            draft_count = sum("noindex" in block_pages[block].robots for block in block_indexes)
            check(sidebar[1].count('class="badge badge-draft">準備中</span>') == draft_count,
                  f"{route}: wrong number of draft badges in the Index")
            if block_routes:
                position = expected_links.index(block_routes[0]) + 1
                expected_links[position:position] = block_routes
            check(sidebar_links == expected_links, f"{route}: wrong Index order: {sidebar_links}")
            current = re.findall(r'<a href="([^"]+)" aria-current="page">', sidebar[1])
            check(len(current) == 1 and local_targets(route, f' href="{current[0]}"') == [route],
                  f"{route}: Index must mark the current page")
            check("block-field-nav-wide" not in page.source, f"{route}: duplicate desktop field navigation")
            if route != field:
                check(re.search(r'</h1>\s*<nav class="block-field-nav block-field-nav-narrow"', page.source) is not None,
                      f"{route}: mobile field navigation must immediately follow the title")
            if route not in public:
                continue
            cards = list(re.finditer(r'<nav class="chapter-nav-card">.*?</nav>', page.source, re.S))
            check(len(cards) == 2 and cards[0][0] == cards[1][0], f"{route}: expected two identical navigation cards")
            check(page.source.index("</h1>") < cards[0].start() < cards[1].start()
                  and re.match(r"\s*</article>", page.source[cards[1].end():]) is not None,
                  f"{route}: navigation cards must bracket the page body")
            for direction, label, edges in (("prev", "← Previous", previous), ("next", "Next →", following)):
                link = re.search(rf'<a class="{direction}" href="[^"]+"[^>]*>\s*'
                                 rf'<span class="nav-label">{label}</span>\s*'
                                 r'<span class="nav-title">([^<]+)</span>', cards[0][0])
                check(link is not None, f"{route}: missing {label} on navigation card")
                target, = local_targets(route, link[0])
                edges[route] = target
                expected_title = titles[target]
                if target == field:
                    expected_title += "の目次"
                elif target.rsplit("/", 1)[0] != route.rsplit("/", 1)[0]:
                    target_block = target.rsplit("/", 1)[0] + "/index.html"
                    if titles[target_block] != expected_title:
                        expected_title += f" ({titles[target_block]})"
                check(link[1] == expected_title, f"{route}: wrong title for {target}: {link[1]}")
        for ordered, edges in ((public, following), (public[::-1], previous)):
            if not ordered:
                continue
            route = ordered[0]
            for expected in ordered:
                check(route == expected, f"{field}: expected {expected}, reached {route}")
                route = edges[route]
            check(route == field, f"{field}: last card should return to the field index")


def check_html(site: Path) -> None:
    expected = OPEN_ROUTES | CLOSED_ROUTES
    actual = {
        path.relative_to(site).as_posix()
        for group in (REGRESSION, INFERENCE, PRACTICE, STATISTICS, DRAFT)
        for path in (site / group).rglob("*.html")
    }
    check(actual == expected, f"Missing routes: {sorted(expected - actual)}. Extra routes: {sorted(actual - expected)}.")
    pages = {path: Page(site, path) for path in sorted(expected)}

    for path, page in pages.items():
        is_block = path.startswith("fields/") and path.count("/") == 3
        if is_block or path in {GUIDE, CHECKLIST, DRAFT_GUIDE}:
            page.check_toc_targets()
        for link in page.links:
            target, separator, fragment = link.partition("#")
            if separator and target in pages:
                check(fragment in pages[target].ids, f"{path}: missing target ID in {link}")
        check(("noindex" in page.robots) == (path in CLOSED_ROUTES), f"{path}: wrong robots metadata")
        if path in CLOSED_ROUTES:
            check("準備中" in page.source, f"{path}: missing closed-page notice")
    for path in sorted(DRAFT_ROUTES):
        page = pages[path]
        labels = ("準備講義の資料",) if path == DRAFT_GUIDE else ("統計の準備講義の説明です.",)
        for text in ('class="placeholder"', "統計の準備講義", *labels):
            check(text in page.source, f"{path}: missing {text}")
        for text in ("準備講義の第1回", "準備講義の補足", "course-sessions", "session-blocks",
                     "session-exercises", "course-materials", "draft-1.py", 'class="protected"'):
            check(text not in page.source, f"{path}: contains {text}")
        allowed = {f"{DRAFT}/index.html"} if path == DRAFT_GUIDE else set()
        check(not page.links & ((DRAFT_ROUTES | {OVERVIEW, PREDICTION}) - allowed),
              f"{path}: links to a session or block in the closed course")
    pages[DRAFT_GUIDE].has_links(f"{DRAFT}/index.html")
    for anchor in ("draft-heading", "draft-answer"):
        check(f'<span id="{anchor}"></span>' in pages[DRAFT_GUIDE].source,
              f"{DRAFT_GUIDE}: missing empty anchor {anchor}")
    check_no_marker(site, "FIXTURE_CLOSED_")
    check("準備中" in pages[PRACTICE_1].source, f"{PRACTICE_1}: missing closed-block notice")
    check("準備中" in pages[f"{REGRESSION}/index.html"].source, "Field index lacks closed-block notice")
    prerequisites = pages[EXTENSION].source.split('class="block-prerequisites"', 1)[1].split("</nav>", 1)[0]
    check(pages[EXTENSION].prerequisites == [PREDICTION, SAMPLING]
          and prerequisites.count('class="badge badge-draft">準備中') == 1
          and "準備中" in prerequisites.split("</li>", 1)[0],
          f"{EXTENSION}: wrong prerequisite order or badge")
    escaped_block = escape("標本の取り方 <A> & <B>")
    check(f">{escaped_block}</a>" in prerequisites, f"{EXTENSION}: prerequisite title was not escaped")
    for path in (OVERVIEW, CHECK_SAMPLE, PREDICTION):
        check('class="block-prerequisites"' not in pages[path].source,
              f"{path}: unexpected prerequisite list")
    field = pages[f"{REGRESSION}/index.html"].source
    escaped_heading = escape("予測に使う標本 <A> & <B>")
    check(f">{escaped_heading}</a>" in field, "Field index lacks the escaped first top-level heading")
    check('class="nav-title">予測の復習用ブロック</span>' in pages[CHECK_SAMPLE].source,
          "Page navigation lacks the page title")
    check('class="article-title">予測の復習用ブロック</h1>' in pages[EXTENSION].source,
          "The index heading changed the block title")
    check(">導入</a>" in field and ">回帰分析の補足</a>" not in field,
          "A heading inside a note changed the index label")

    pages[OVERVIEW].has_links(f"{VARIABLES}#variables", f"{VARIABLES}#observation", f"{FIT}#数式")
    pages[VARIABLES].has_links(FITTING)
    pages[FITTING].has_links(f"{FIT}#数式", f"{EXERCISES}#exercise-sample-fitting-1")
    pages[UNCERTAINTY].has_links(f"{VARIABLES}#variables", f"{SAMPLE}#sample")
    pages[EXERCISES].has_links(f"{FIT}#fit")
    pages[PRACTICE_1].has_links(f"{FIT}#数式", f"{EXERCISES}#exercise-sample-fitting-1",
                                CHECKLIST, f"{PRACTICE_1}#session-intro")
    pages[STATISTICS_1].has_links(f"{EXERCISES}#exercise-sample-fitting-1")
    pages[PRACTICE_INDEX].has_links(PRACTICE_1, PRACTICE_2, GUIDE, CHECKLIST, FOLLOW_UP)
    index = pages[PRACTICE_INDEX].source
    escaped_guide_link = f">{escape('実践の進め方 <A> & <B>')}</a>"
    check(escaped_guide_link in index, f"{PRACTICE_INDEX}: material title was not escaped")
    check(index.index(escaped_guide_link) < index.index(">提出前の確認</a>")
          < index.index('class="course-sessions"') < index.index(">予測の復習</a></h2>"),
          "Course materials, numbered sessions and supplements are out of order")
    for path in (PRACTICE_1, FOLLOW_UP):
        content = pages[path].source
        check(content.index('class="course-materials"') < content.index('class="session-blocks"'),
              f"{path}: materials must precede blocks")
    supplements_materials = pages[FOLLOW_UP].source.split('class="course-materials"', 1)[1].split("</ul>", 1)[0]
    check(escaped_guide_link in supplements_materials, f"{FOLLOW_UP}: material title was not escaped")
    pages[FOLLOW_UP].has_links(f"{GUIDE}#予測", CHECKLIST, GUIDE, EXTENSION)
    pages[GUIDE].has_links(PRACTICE_INDEX, f"{GUIDE}#予測", f"{CHECKLIST}#submission", f"{FIT}#fit")
    pages[CHECKLIST].has_links(f"{GUIDE}#予測")
    for text in ('class="math inline"', 'class="sourceCode python"', 'class="protected"', 'id="lecture-toc" data-mode="single"'):
        check(text in pages[GUIDE].source, f"{GUIDE}: missing {text}")
    guide_headings = pages[GUIDE].headings
    check([heading for heading in guide_headings if heading[0] == "h1"] == [("h1", None)]
          and '<h1 class="article-title">' in pages[GUIDE].source,
          f"{GUIDE}: only the page title should be h1")
    for heading in (("h2", "当てはめ"), ("h3", "model-details"), ("h2", "bibliography")):
        check(heading in guide_headings, f"{GUIDE}: expected {heading}")
    pages[f"{STATISTICS}/index.html"].has_links(STATISTICS_1, STATISTICS_2)
    pages[f"{REGRESSION}/index.html"].has_links(OVERVIEW, FITTING)
    pages[f"{INFERENCE}/index.html"].has_links(SAMPLING, UNCERTAINTY, OVERVIEW)
    check("practice-1.py" in pages[PRACTICE_1].source, f"{PRACTICE_1}: missing assignment filename")
    check("statistics-1.py" in pages[STATISTICS_1].source, f"{STATISTICS_1}: missing assignment filename")
    for session, expected_rows in {
        PRACTICE_1: [(SAMPLING, "必修"), (OVERVIEW, "必修"), (FITTING, "必修"), (PREDICTION, "準備中")],
        PRACTICE_2: [(OVERVIEW, "発展"), (UNCERTAINTY, "必修")],
        FOLLOW_UP: [(EXTENSION, "必修")],
        STATISTICS_1: [(OVERVIEW, "必修"), (FITTING, "発展")],
        STATISTICS_2: [(SAMPLING, "必修"), (UNCERTAINTY, "必修")],
    }.items():
        rows = pages[session].session_rows
        check(len(rows) == len(expected_rows), f"{session}: wrong block count")
        for (links, text), (target, label) in zip(rows, expected_rows):
            check(target in links and label in "".join(text),
                  f"{session}: expected {target} with label {label}")
    check("class=\"protected\"" in pages[EXERCISES].source, f"{EXERCISES}: missing protected answer")
    check("note-heading" in pages[VARIABLES].ids, f"{VARIABLES}: missing nested heading from Div")
    check("コードフェンス内" in pages[VARIABLES].source, f"{VARIABLES}: missing fenced code")
    check("fenced-heading" not in pages[VARIABLES].ids, f"{VARIABLES}: fenced text became a heading")
    for path, page in pages.items():
        check(("refs" in page.ids) == (path in {FIT, GUIDE}), f"{path}: wrong bibliography placement")
    for path in (FIT, GUIDE):
        check("ref-james2021introduction" in pages[path].ids, f"{path}: cited entry is missing")

    overview = pages[OVERVIEW]
    expected_neighbors = {
        PRACTICE_1: (SAMPLING, FITTING),
        PRACTICE_2: (PREDICTION, UNCERTAINTY),
        STATISTICS_1: (None, FITTING),
    }
    check(len(overview.occurrences) == len(expected_neighbors), f"{OVERVIEW}: wrong occurrence count")
    for path in (OVERVIEW, PREDICTION):
        check(not pages[path].links & DRAFT_ROUTES, f"{path}: links to the closed course")
    for session, neighbors in expected_neighbors.items():
        matching = [occ for occ in overview.occurrences if session in occ.values()]
        check(len(matching) == 1, f"{OVERVIEW}: missing or duplicate occurrence for {session}")
        occurrence = matching[0]
        check((occurrence.get("prev"), occurrence.get("next")) == neighbors,
              f"{OVERVIEW}: wrong neighbors for {session}: {occurrence}")
    supplement, = pages[EXTENSION].occurrences
    check(FOLLOW_UP in supplement and supplement.get("prev") == UNCERTAINTY and "next" not in supplement,
          f"{EXTENSION}: incorrect supplement placement")
    check(">予測の復習</a> (必修)" in pages[EXTENSION].source and "第 3 回" not in pages[EXTENSION].source,
          f"{EXTENSION}: supplement was numbered as a session")
    check(any(PRACTICE_2 in occ and occ.get("next") == EXTENSION for occ in pages[UNCERTAINTY].occurrences),
          f"{UNCERTAINTY}: missing next block from supplement")


def check_no_marker(site: Path, marker: str) -> None:
    for pattern in ("*.html", "*.xml"):
        for path in site.rglob(pattern):
            check(marker not in path.read_text(encoding="utf-8"),
                  f"{path.relative_to(site)}: contains {marker}")


def sitemap_paths(site: Path) -> list[str]:
    return [
        urlsplit(location.text or "").path.lstrip("/")
        for location in ElementTree.parse(site / "sitemap.xml").findall(".//{*}loc")
    ]


def check_feeds(site: Path) -> None:
    locations = sitemap_paths(site)
    for path in sorted(OPEN_ROUTES):
        check(locations.count(path) == 1, f"sitemap must list {path} once")
    for path in sorted(CLOSED_ROUTES):
        check(path not in locations, f"sitemap includes closed page {path}")

    tags = list((site / "tags").glob("*.html"))
    tag_feeds = list((site / "tags").glob("*.xml"))
    check(bool(tags and tag_feeds), "Existing tag pages or feeds are missing")
    check(not any(path.stem == "fixture-block-regression" for path in tags + tag_feeds),
          "Block tag generated a tag page or feed")
    route_pattern = re.compile("|".join(map(re.escape, sorted(OPEN_ROUTES | CLOSED_ROUTES))))
    for path in [site / "rss.xml", *tags, *tag_feeds]:
        content = path.read_text(encoding="utf-8")
        check(route_pattern.search(content) is None, f"{path.name}: includes fixture URL")

    rss = ElementTree.parse(site / "rss.xml")
    posts = [urlsplit(item.text or "").path for item in rss.findall("./channel/item/link")]
    check(bool(posts) and all(path.startswith("/posts/") for path in posts), "The main RSS must contain posts only")


def check_course_closure(executable: Path, root: Path, site: Path) -> None:
    path = root / "fixtures" / f"{PRACTICE}.yaml"
    opened = path.read_text(encoding="utf-8")
    check(opened.count("open: true") == 1, f"{path}: expected one open flag")
    for command in ("build", "rebuild"):
        if command == "rebuild":
            path.write_text(opened, encoding="utf-8")
            subprocess.run([str(executable), "build"], cwd=root, env=FIXTURE_ENV, check=True)
        # Each closure starts with these routes publicly rendered in the same output directory.
        locations = sitemap_paths(site)
        for route, markers in PRACTICE_CONTENT.items():
            page = Page(site, route)
            check("noindex" not in page.robots and route in locations, f"{route}: not public before {command}")
            for marker in markers:
                check(marker in page.source, f"{route}: missing {marker} before {command}")

        path.write_text(opened.replace("open: true", "open: false", 1), encoding="utf-8")
        subprocess.run([str(executable), command], cwd=root, env=FIXTURE_ENV, check=True)
        locations = sitemap_paths(site)
        check_no_marker(site, "FIXTURE_OPEN_")
        check_no_marker(site, "FIXTURE_CLOSED_")
        for route in PRACTICE_CONTENT:
            page = Page(site, route)
            check("noindex" in page.robots and "準備中" in page.source, f"{route}: not closed after {command}")
            check(route not in locations, f"{route}: still in sitemap after {command}")
            for marker in (*PRACTICE_CONTENT[PRACTICE_INDEX], 'class="session-blocks"', 'class="protected"'):
                check(marker not in page.source, f"{route}: still contains {marker} after {command}")


def check_invalid(
    executable: Path, source: str, original: str, replacement: str, *diagnostics: str,
    reported_by: str | None = None,
) -> None:
    with tempfile.TemporaryDirectory(prefix="block-site-invalid-") as directory:
        root = Path(directory)
        shutil.copytree(REPO / "fixtures", root / "fixtures")
        shutil.copytree(REPO / "bib", root / "bib")
        path = root / "fixtures" / source
        text = path.read_text(encoding="utf-8")
        check(text.count(original) == 1, f"Mutation must match once in {source}: {original!r}")
        path.write_text(text.replace(original, replacement, 1), encoding="utf-8")
        expect_failed_build(executable, root, f"fixtures/{reported_by or source}", *diagnostics)


def expect_failed_build(executable: Path, root: Path, *diagnostics: str) -> None:
    result = subprocess.run(
        [str(executable), "build"], cwd=root, env=FIXTURE_ENV,
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, timeout=60,
    )
    expected = ", ".join(diagnostics)
    check(result.returncode != 0, f"Expected build failure reporting {expected}")
    missing = [fragment for fragment in diagnostics if fragment not in result.stdout]
    check(not missing, f"Expected diagnostic fragments {missing}:\n{result.stdout}")


def check_invalid_inputs(executable: Path) -> None:
    overview = f"{REGRESSION}/sample-overview.md"
    fitting = f"{REGRESSION}/sample-fitting.md"
    practice = f"{PRACTICE}.yaml"
    draft = f"{DRAFT}.yaml"
    variables = "# 説明変数 {#variables}"
    check_invalid(
        executable, overview, "open: true", "open: true\nrequires: [sample-fitting]",
        "requires cycle", fitting,
    )
    check_invalid(executable, overview, variables, "# 説明変数", "expected explicit", "ID")
    check_invalid(
        executable, practice,
        "- block: sample-overview\n        level: required", "- block: sample-overview\n        level: advanced",
        "required block sample-fitting requires earlier required block sample-overview",
    )
    check_invalid(
        executable, fitting, "# 演習 {#exercises}",
        "::: {#refs}\n:::\n\n# 演習 {#exercises}", "reserved bibliography ID #refs",
    )
    check_invalid(
        executable, f"{REGRESSION}/field.yaml", "  - sample-prediction\n", "",
        "block is missing from field.yaml", reported_by=f"{REGRESSION}/sample-prediction.md",
    )
    check_invalid(
        executable, f"{REGRESSION}/field.yaml", "  - sample-prediction", "  - sample-prediction\n  - sample-missing",
        "missing block file: sample-missing",
    )
    check_invalid(
        executable, overview, "block:sample-fitting#数式", "block:sample-fitting#missing",
        "unknown ID: block:sample-fitting#missing",
    )
    check_invalid(
        executable, overview, variables, f"# 索引 {{#index}}\n\n{variables}",
        f"duplicate output path {OVERVIEW}",
    )
    check_invalid(
        executable, practice, "id: exercise-sample-fitting-1", "id: fit",
        "unknown exercise sample-fitting#fit",
    )
    for value in ('"false"', "null"):
        check_invalid(executable, draft, "open: false", f"open: {value}", "$.open", "expected Bool")
    check_invalid(executable, draft, "block: sample-overview\n        level: required",
                  "block: sample-missing\n        level: required",
                  "unknown block sample-missing")
    check_invalid(executable, draft, "id: exercise-sample-prediction-1", "id: forecast",
                  "unknown exercise sample-prediction#forecast")
    for original, broken, diagnostic in (
        ("(#draft-session-intro)", "(#missing-session)", "unknown local ID: #missing-session"),
        ("(block:sample-overview#variables)", "(block:sample-overview#missing)",
         "unknown ID: block:sample-overview#missing"),
        ("(#draft-supplement-intro)", "(#missing-supplement)", "unknown local ID: #missing-supplement"),
        ("(block:sample-sampling#sample)", "(block:sample-sampling#missing)",
         "unknown ID: block:sample-sampling#missing"),
    ):
        check_invalid(executable, draft, original, broken, diagnostic)
    for names, duplicate, route in (
        ("[guide, checklist]", "guide", PRACTICE_INDEX),
        ("[checklist]", "checklist", PRACTICE_1),
        ("[checklist, guide]", "guide", FOLLOW_UP),
    ):
        check_invalid(executable, practice, f"materials: {names}",
                      f"materials: {names[:-1]}, {duplicate}]",
                      f"duplicate material in {route} {duplicate}")
    check_invalid(executable, practice, "materials: [guide, checklist]", "materials: [guide, checklist, missing]",
                  "missing material file: missing")
    for key in ("guide", "index", "1"):
        check_invalid(executable, practice, "id: follow-up", f"id: '{key}'",
                      f"duplicate output path {PRACTICE}/{key}.html")
    check_invalid(executable, practice, "id: follow-up", "id: Bad_name", "invalid supplement id")
    check_invalid(executable, practice, "  - id: follow-up", "  - {id: follow-up, title: 復習, blocks: []}\n  - id: follow-up",
                  f"duplicate output path {FOLLOW_UP}")
    for key, diagnostic in (
        ("index", f"duplicate output path {PRACTICE_INDEX}"),
        ("1", f"duplicate output path {PRACTICE_1}"),
        ("follow-up", f"duplicate output path {FOLLOW_UP}"),
        ("Bad_name", "invalid material name"),
        ("orphan", "material is missing from"),
    ):
        with tempfile.TemporaryDirectory(prefix="block-site-material-name-") as directory:
            root = Path(directory)
            shutil.copytree(REPO / "fixtures", root / "fixtures")
            bad_material = root / "fixtures" / PRACTICE / f"{key}.md"
            bad_material.write_text("---\ntitle: 確認\n---\n", encoding="utf-8")
            expect_failed_build(executable, root, diagnostic)
    guide_title = "title: 実践の進め方 <A> & <B>"
    check_invalid(executable, f"{PRACTICE}/guide.md", guide_title, "open: true",
                  "material inherits open from its course")
    check_invalid(executable, f"{PRACTICE}/guide.md", guide_title, "description: 資料",
                  'key "title" not found')
    check_invalid(executable, practice, "material:guide#予測", "material:guide#missing",
                  "unknown ID: material:guide#missing")
    check_invalid(executable, draft, "material:guide#draft-heading", "material:guide#missing",
                  "unknown ID: material:guide#missing")
    check_invalid(executable, draft, "(material:guide)", "(material:checklist)",
                  "unknown material link: material:checklist")
    check_invalid(executable, f"{DRAFT}/guide.md", "block:sample-overview", "block:sample-overview#missing",
                  "unknown ID: block:sample-overview#missing")
    check_invalid(executable, f"{DRAFT}/guide.md", "block:sample-overview", "material:guide#missing",
                  "unknown ID: material:guide#missing")
    check_invalid(executable, f"{DRAFT}/guide.md", "block:sample-overview", "#missing",
                  "unknown local ID: #missing")
    check_invalid(executable, f"{PRACTICE}/guide.md", "material:checklist#submission", "material:sample-draft/guide",
                  "unknown material link: material:sample-draft/guide")
    check_invalid(executable, overview, "block:sample-fitting#数式", "material:guide",
                  "material link outside its course: material:guide")
    check_invalid(executable, practice, "block: sample-prediction\n        level: required",
                  "block: sample-prediction\n        level: advanced",
                  "required block sample-extension requires earlier required block sample-prediction")
    with tempfile.TemporaryDirectory(prefix="block-site-legacy-collision-") as directory:
        root = Path(directory)
        lectures = ("lectures/one/sample-duplicate.md", "lectures/two/sample-duplicate.md")
        for source in lectures:
            path = root / source
            path.parent.mkdir(parents=True)
            path.write_text("---\ntitle: Sample lecture\n---\n\nSample.\n", encoding="utf-8")
        expect_failed_build(
            executable, root, "duplicate output path lectures/sample-duplicate.html", *lectures,
        )


def main() -> int:
    executable = Path(subprocess.check_output(
        ["stack", "exec", "--", "which", "main"], cwd=REPO, text=True,
    ).strip())
    with tempfile.TemporaryDirectory(prefix="block-site-") as directory:
        root = Path(directory)
        # Match the full build without writing generated pages into the checkout.
        for name in ("bib", "courses", "css", "favicon.ico", "fields", "files",
                     "google444b04943c3fafb4.html", "images", "lectures", "pages", "posts",
                     "slds_code", "slds_data", "slds_papers", "templates"):
            source = REPO / name
            if source.exists():
                (root / name).symlink_to(source, target_is_directory=source.is_dir())
        shutil.copytree(REPO / "fixtures", root / "fixtures")
        subprocess.run([str(executable), "build"], cwd=root, env=FIXTURE_ENV, check=True)
        site = root / "_fixture_site"
        check_reference_material_names(site)
        check_reference_navigation(site)
        check_html(site)
        check_feeds(site)
        subprocess.run([sys.executable, str(REPO / "scripts/check_local_refs.py"), str(site)], check=True)
        check_course_closure(executable, root, site)
    check_invalid_inputs(executable)
    print("Block site acceptance checks passed.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
