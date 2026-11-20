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
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urljoin, urlsplit
from xml.etree import ElementTree

REPO = Path(__file__).resolve().parent.parent
SITE = REPO / "_fixture_site"
FIXTURE_ENV = {**os.environ, "BLOG_FIXTURES": "1"}

REGRESSION = "fields/sample-regression"
INFERENCE = "fields/sample-inference"
PRACTICE = "courses/sample-practice"
STATISTICS = "courses/sample-statistics"
OVERVIEW = f"{REGRESSION}/sample-overview/index.html"
VARIABLES = f"{REGRESSION}/sample-overview/variables.html"
FITTING = f"{REGRESSION}/sample-fitting/index.html"
FIT = f"{REGRESSION}/sample-fitting/fit.html"
RESIDUALS = f"{REGRESSION}/sample-fitting/residuals.html"
EXERCISES = f"{REGRESSION}/sample-fitting/exercises.html"
PREDICTION = f"{REGRESSION}/sample-prediction/index.html"
SAMPLING = f"{INFERENCE}/sample-sampling/index.html"
SAMPLE = f"{INFERENCE}/sample-sampling/sample.html"
UNCERTAINTY = f"{INFERENCE}/sample-uncertainty/index.html"
PRACTICE_1 = f"{PRACTICE}/1.html"
PRACTICE_2 = f"{PRACTICE}/2.html"
STATISTICS_1 = f"{STATISTICS}/1.html"
STATISTICS_2 = f"{STATISTICS}/2.html"

OPEN_ROUTES = {
    f"{REGRESSION}/index.html",
    f"{INFERENCE}/index.html",
    OVERVIEW, VARIABLES, FITTING, FIT, RESIDUALS, EXERCISES,
    SAMPLING, SAMPLE, UNCERTAINTY, f"{INFERENCE}/sample-uncertainty/variation.html",
    f"{PRACTICE}/index.html", PRACTICE_1, PRACTICE_2,
    f"{STATISTICS}/index.html", STATISTICS_1, STATISTICS_2,
}
CLOSED_ROUTES = {
    PREDICTION,
    f"{REGRESSION}/sample-prediction/forecast.html",
    f"{REGRESSION}/sample-prediction/exercises.html",
}


def check(condition: bool, message: str) -> None:
    if not condition:
        raise AssertionError(message)


class Page(HTMLParser):
    """Collect IDs, local links and links grouped by navigation element."""

    def __init__(self, path: str) -> None:
        super().__init__()
        self.path = path
        self.source = (SITE / path).read_text(encoding="utf-8")
        self.ids: set[str] = set()
        self.robots: set[str] = set()
        self.links: set[str] = set()
        self.nav: dict[str, dict[str, str]] = {}
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
            self.ids.add(anchor)
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
            rel = attrs.get("rel")
            if rel:
                for navigation in self.nav_stack:
                    for name in navigation:
                        self.nav.setdefault(name, {})[rel] = target
            if self.items and self.items[-1] is not None:
                self.items[-1][rel or target] = target
            if self.session_depth is not None and len(self.items) > self.session_depth:
                self.session_rows[-1][0].add(target)

    def handle_endtag(self, tag: str) -> None:
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


def check_html() -> None:
    expected = OPEN_ROUTES | CLOSED_ROUTES
    actual = {
        path.relative_to(SITE).as_posix()
        for group in (REGRESSION, INFERENCE, PRACTICE, STATISTICS)
        for path in (SITE / group).rglob("*.html")
    }
    check(actual == expected, f"Missing routes: {sorted(expected - actual)}. Extra routes: {sorted(actual - expected)}.")
    pages = {path: Page(path) for path in sorted(expected)}

    for path, page in pages.items():
        for link in page.links:
            target, separator, fragment = link.partition("#")
            if separator and target in pages:
                check(fragment in pages[target].ids, f"{path}: missing target ID in {link}")
        check("FIXTURE_CLOSED_" not in page.source, f"{path}: contains closed content")
        check(("noindex" in page.robots) == (path in CLOSED_ROUTES), f"{path}: wrong robots metadata")
        if path in CLOSED_ROUTES:
            check("準備中" in page.source, f"{path}: missing closed-page notice")
    check("準備中" in pages[PRACTICE_1].source, f"{PRACTICE_1}: missing closed-block notice")
    check("準備中" in pages[f"{REGRESSION}/index.html"].source, "Field index lacks closed-block notice")

    pages[OVERVIEW].has_links(f"{VARIABLES}#variables", f"{VARIABLES}#observation", f"{FIT}#数式")
    pages[VARIABLES].has_links(FITTING)
    pages[FITTING].has_links(f"{FIT}#数式", f"{EXERCISES}#exercise-sample-fitting-1")
    pages[UNCERTAINTY].has_links(f"{VARIABLES}#variables", f"{SAMPLE}#sample")
    pages[EXERCISES].has_links(f"{FIT}#fit")
    pages[PRACTICE_1].has_links(f"{FIT}#数式", f"{EXERCISES}#exercise-sample-fitting-1")
    pages[STATISTICS_1].has_links(f"{EXERCISES}#exercise-sample-fitting-1")
    pages[f"{PRACTICE}/index.html"].has_links(PRACTICE_1, PRACTICE_2)
    pages[f"{STATISTICS}/index.html"].has_links(STATISTICS_1, STATISTICS_2)
    pages[f"{REGRESSION}/index.html"].has_links(OVERVIEW, FITTING)
    pages[f"{INFERENCE}/index.html"].has_links(SAMPLING, UNCERTAINTY, OVERVIEW)
    check("practice-1.py" in pages[PRACTICE_1].source, f"{PRACTICE_1}: missing assignment filename")
    check("statistics-1.py" in pages[STATISTICS_1].source, f"{STATISTICS_1}: missing assignment filename")
    for session, expected_rows in {
        PRACTICE_1: [(SAMPLING, "必修"), (OVERVIEW, "必修"), (FITTING, "必修"), (PREDICTION, "準備中")],
        PRACTICE_2: [(OVERVIEW, "発展"), (UNCERTAINTY, "必修")],
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
        check(("refs" in page.ids) == (path == FIT), f"{path}: wrong bibliography placement")
    check("ref-james2021introduction" in pages[FIT].ids, f"{FIT}: cited entry is missing")

    variables = pages[VARIABLES]
    check(variables.nav.get("block-page-nav") == {"prev": OVERVIEW},
          f"{VARIABLES}: wrong within-block navigation")
    overview = pages[OVERVIEW]
    check(overview.nav.get("block-page-nav") == {"next": VARIABLES},
          f"{OVERVIEW}: wrong within-block navigation")
    expected_neighbors = {
        PRACTICE_1: (SAMPLING, FITTING),
        PRACTICE_2: (PREDICTION, UNCERTAINTY),
        STATISTICS_1: (None, FITTING),
    }
    check(len(overview.occurrences) == len(expected_neighbors), f"{OVERVIEW}: wrong occurrence count")
    for session, neighbors in expected_neighbors.items():
        matching = [occ for occ in overview.occurrences if session in occ.values()]
        check(len(matching) == 1, f"{OVERVIEW}: missing or duplicate occurrence for {session}")
        occurrence = matching[0]
        check((occurrence.get("prev"), occurrence.get("next")) == neighbors,
              f"{OVERVIEW}: wrong neighbors for {session}: {occurrence}")


def check_feeds() -> None:
    sitemap = ElementTree.parse(SITE / "sitemap.xml")
    locations = [
        urlsplit(location.text or "").path.lstrip("/")
        for location in sitemap.findall(".//{*}loc")
    ]
    for path in sorted(OPEN_ROUTES):
        check(locations.count(path) == 1, f"sitemap must list {path} once")
    for path in sorted(CLOSED_ROUTES):
        check(path not in locations, f"sitemap includes closed page {path}")

    tags = list((SITE / "tags").glob("*.html"))
    tag_feeds = list((SITE / "tags").glob("*.xml"))
    check(bool(tags and tag_feeds), "Existing tag pages or feeds are missing")
    check(not any(path.stem == "fixture-block-regression" for path in tags + tag_feeds),
          "Block tag generated a tag page or feed")
    route_pattern = re.compile("|".join(map(re.escape, sorted(OPEN_ROUTES | CLOSED_ROUTES))))
    for path in [SITE / "rss.xml", *tags, *tag_feeds]:
        content = path.read_text(encoding="utf-8")
        check(route_pattern.search(content) is None, f"{path.name}: includes fixture URL")
        check("FIXTURE_CLOSED_" not in content, f"{path.name}: includes closed content")

    rss = ElementTree.parse(SITE / "rss.xml")
    posts = [urlsplit(item.text or "").path for item in rss.findall("./channel/item/link")]
    check(bool(posts) and all(path.startswith("/posts/") for path in posts), "The main RSS must contain posts only")


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
    subprocess.run(["stack", "exec", "main", "rebuild"], cwd=REPO, env=FIXTURE_ENV, check=True)
    check_html()
    check_feeds()
    subprocess.run([sys.executable, str(REPO / "scripts/check_local_refs.py"), str(SITE)], check=True)

    executable = Path(subprocess.check_output(
        ["stack", "exec", "--", "which", "main"], cwd=REPO, text=True,
    ).strip())
    check_invalid_inputs(executable)
    print("Block site acceptance checks passed.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
