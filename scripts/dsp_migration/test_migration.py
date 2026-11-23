"""Exercise the migration checks with deliberate damage to a temporary copy."""

from __future__ import annotations

from collections import Counter, defaultdict
from contextlib import redirect_stderr, redirect_stdout
import csv
from io import StringIO
from pathlib import Path
import shutil
import sys
import tempfile
import unittest
from unittest.mock import patch
from urllib.parse import urlsplit

from course import COURSE_YAML, material_path, verify_course
from markdown import MigrationError, RawTags, front_matter, locate_urls, nodes, parse, raw_content
from migrate import FIELDS, Migration, ROOT, TABLES, lecture_chapter, main as migrate_main
from verify_site import LinkTargets, Section, article_body, compare_block, regression_links, sections


def migrate_cli(*args):
    error = StringIO()
    with patch.object(sys, "argv", ["migrate.py", *map(str, args)]), redirect_stderr(error), redirect_stdout(StringIO()):
        status = migrate_main()
    return status, error.getvalue()


class AddedLinkTableTests(unittest.TestCase):
    def test_matches_three_approved_links(self):
        approved = (
            (7, "アソシエーション分析", "block:ml-association"),
            (8, "Transformer と注意機構", "block:dl-transformer"),
            (11, "画像認識を実行", "block:img-age#age-classification"),
        )
        with (TABLES / "added_links.tsv").open(encoding="utf-8", newline="") as source:
            actual = [
                (int(row["chapter"]), row["label"], row["target"])
                for row in csv.DictReader(source, delimiter="\t")
            ]
        self.assertCountEqual(actual, approved)


class MigrationTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        folder = tempfile.TemporaryDirectory(prefix="dsp-migration-test-")
        cls.addClassCleanup(folder.cleanup)
        cls.root = Path(folder.name)
        shutil.copytree(ROOT / "lectures/dsp", cls.root / "lectures/dsp")
        shutil.copytree(ROOT / "fields/regression", cls.root / "fields/regression")
        shutil.copytree(ROOT / "courses", cls.root / "courses")
        (cls.root / "pages").mkdir()
        shutil.copyfile(ROOT / "pages/lectures.markdown", cls.root / "pages/lectures.markdown")
        (cls.root / "scripts").mkdir()
        shutil.copyfile(ROOT / "scripts/reg_old_anchors.tsv", cls.root / "scripts/reg_old_anchors.tsv")
        cls.layout = (TABLES / "layout.tsv").read_text(encoding="utf-8")
        cls.migration = Migration(cls.root)
        cls.outputs = dict(cls.migration.outputs())

    def with_layout(self, content):
        path = self.root / "layout.tsv"
        path.write_text(content, encoding="utf-8")
        return Migration(self.root, layout=path)

    def replace_once(self, text, old, new):
        self.assertIn(old, text, "改変する箇所を原稿で確認してください")
        return text.replace(old, new, 1)

    def verify_mutation(self, block_name, old, new, message):
        block = self.migration.blocks[block_name]
        damaged = self.replace_once(self.outputs[block.path()], old, new)
        with self.assertRaisesRegex(MigrationError, message):
            self.migration.verify_body(block, damaged)

    def test_current_layout_and_contents(self):
        self.assertEqual(len(self.migration.blocks), 45)
        self.assertEqual({block.field for block in self.migration.blocks.values()}, FIELDS.keys())
        self.assertEqual(len(self.outputs), 55)
        self.assertEqual(self.migration.provisional, [])
        self.assertEqual(dict(self.migration.outputs()), self.outputs)
        counts = Counter()
        for block in self.migration.blocks.values():
            text = self.outputs[block.path()]
            self.assertIn("\nopen: true\n", text)
            counts.update(self.migration.verify_body(block, text))
        self.assertEqual((counts["Exercise"], counts["Answer"], counts["CodeBlock"]), (28, 28, 320))

    def test_open_rejects_the_script_checkout_before_any_write(self):
        with tempfile.TemporaryDirectory() as folder:
            directory = Path(folder)
            checkout = directory / "checkout"
            shutil.copytree(self.root, checkout)
            alias = directory / "alias"
            alias.symlink_to(checkout, target_is_directory=True)
            manifest = directory / "manifest.json"
            before = {path: path.read_bytes() for path in checkout.rglob("*") if path.is_file()}
            with patch("migrate.ROOT", checkout.resolve()):
                for root_arg in ([], ["--root", alias]):
                    with self.subTest(root_arg=root_arg):
                        status, error = migrate_cli("generate", "--open", "--manifest", manifest, *root_arg)
                        self.assertEqual(status, 1)
                        self.assertIn("--open はビルド用の写しにだけ", error)
                        self.assertFalse(manifest.exists())
                        after = {path: path.read_bytes() for path in checkout.rglob("*") if path.is_file()}
                        self.assertEqual(before, after)

    def test_field_catalog_preserves_listed_manual_files_and_rejects_bad_catalogs(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder) / "copy"
            shutil.copytree(self.root, root)
            for relative, text in self.outputs.items():
                path = root / relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(text, encoding="utf-8")
            catalog = root / "fields/descriptive/field.yaml"
            assigned = [block.name for block in self.migration.blocks.values() if block.field == "descriptive"]
            manual = catalog.parent / "manual-extension.md"
            original_manual = b"\xffmanual-content-that-must-not-be-read"
            manual.write_bytes(original_manual)
            names = [assigned[0], "'manual-extension'", *assigned[1:]]

            def update_catalog(sequence):
                original = ("---\ntitle: 編集済みの分野名\ndescription: 編集済みの説明\n"
                            f"blocks: [{', '.join(sequence)}] # 手編集\n").encode()
                catalog.write_bytes(original)
                return original

            for case, sequence, message in (
                ("listed manual", names, None),
                ("assigned missing", names[:-1], "割り当て済みのブロックがありません"),
                ("assigned reordered", [names[2], names[1], names[0], *names[3:]], "相対順"),
                ("unlisted manual", assigned, "両方にないファイル"),
            ):
                original_catalog = update_catalog(sequence)
                for mode in ([], ["--check"], ["--verify"]):
                    with self.subTest(case=case, mode=mode):
                        manifest = Path(folder) / "manifest.json"
                        manifest.unlink(missing_ok=True)
                        status, error = migrate_cli("generate", "--root", root, "--manifest", manifest, *mode)
                        self.assertEqual(status, int(message is not None), error)
                        if message:
                            self.assertIn(message, error)
                            self.assertFalse(manifest.exists())
                        self.assertEqual(catalog.read_bytes(), original_catalog)
                        self.assertEqual(manual.read_bytes(), original_manual)

    def test_missing_assignment(self):
        row = next(line for line in self.layout.splitlines() if "\tvis-design\t" in line)
        with self.assertRaisesRegex(MigrationError, "割り当ての欠落"):
            self.with_layout(self.replace_once(self.layout, row + "\n", ""))

    def test_duplicate_assignment(self):
        damaged = self.replace_once(self.layout, "design..度数分布表とヒストグラム",
                                    "line..度数分布表とヒストグラム")
        with self.assertRaisesRegex(MigrationError, "割り当ての重複"):
            self.with_layout(damaged)

    def test_lost_source_id(self):
        original = (self.root / "lectures/dsp/dsp11.md").read_text(encoding="utf-8")
        mutated = self.replace_once(original, "{#cnn}", "{#lost-cnn}")
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            shutil.copytree(self.root / "lectures", root / "lectures")
            (root / "lectures/dsp/dsp11.md").write_text(mutated, encoding="utf-8")
            with self.assertRaisesRegex(MigrationError, "見出しがありません .*#cnn"):
                Migration(root, page_ids=root / "absent.tsv")

    def test_lost_destination_id(self):
        self.verify_mutation("img-cnn", "{#cnn}", "{#lost-cnn}", "構文木に差")

    def test_one_character_change(self):
        self.verify_mutation("design-process", "科学の総称です", "科学の俗称です", "構文木に差")

    def test_broken_link(self):
        block = self.migration.blocks["causal-intro"]
        with self.assertRaisesRegex(MigrationError, "参照する旧 ID がありません"):
            self.migration.target(block, "dsp4.html#missing-anchor-for-test")
        self.verify_mutation("causal-intro", "(block:num-correlation#因果関係と相関)",
                             "(block:num-correlation#missing-anchor-for-test)", "構文木に差")

    def test_code_whitespace(self):
        self.verify_mutation("vis-basic", "\nimport pandas as pd\n", "\nimport  pandas as pd\n", "逐語一致")
        self.verify_mutation("vis-basic", "\nimport seaborn as sns\n~~~", "\nimport seaborn as sns\n\n~~~", "逐語一致")

    def test_shifted_lines_keep_assignments_and_page_ids(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            shutil.copytree(self.root / "lectures", root / "lectures")
            shutil.copytree(self.root / "scripts", root / "scripts")
            shutil.copytree(self.root / "fields/regression", root / "fields/regression")
            path = root / "lectures/dsp/dsp11.md"
            text = path.read_text(encoding="utf-8")
            path.write_text(self.replace_once(text, "\n## 認識技術の活用事例", "\n\n## 認識技術の活用事例"), encoding="utf-8")
            shifted = Migration(root)
        self.assertEqual(shifted.manifest(), self.migration.manifest())
        self.assertEqual(shifted.provisional, self.migration.provisional)

    def test_preserve_fake_links_in_code(self):
        text = (
            '# Title\n\n'
            '`version [text](#fake-inline)` and [real](#title).\n\n'
            '~~~ python\n'
            'message = "[text](#fake-code) <a href=\'fake-file\'>x</a>"\n'
            '~~~\n\n'
            '<a href="#title">Real in HTML</a>\n'
        )
        self.assertEqual([(url.url, url.attribute) for url in locate_urls(text, parse(text))],
                         [("#title", ""), ("#title", "href")])

    def test_table_overrides(self):
        provisional = Migration(self.root, page_ids=self.root / "absent.tsv")
        path = self.root / "provided-ids.tsv"
        block, old, _ = provisional.provisional[0]
        chapter = self.migration.blocks[block].chapter.number
        path.write_text(f"chapter\told_id\tpage_id\n{chapter}\t{old}\tstable-page\n", encoding="utf-8")
        mapped = Migration(self.root, page_ids=path)
        self.assertIn("stable-page", mapped.blocks[block].pages)
        self.assertEqual(len(mapped.provisional), len(provisional.provisional) - 1)

        path.write_text(f"chapter\told_id\tpage_id\n{chapter}\t{old}\tindex\n", encoding="utf-8")
        with self.assertRaisesRegex(MigrationError, "予約済み"):
            Migration(self.root, page_ids=path)

        block_path = self.migration.blocks["design-process"].path()
        manual = self.root / block_path
        manual.parent.mkdir(parents=True, exist_ok=True)
        manual.write_text(self.outputs[block_path], encoding="utf-8")
        self.addCleanup(manual.unlink)
        edited = self.replace_once(self.layout, "2\trequired\t\t\ttrue\ttrue\t\t\n",
                                    "2\trequired\t\t\tfalse\ttrue\t\t\n")
        edited = self.replace_once(edited, "3\trequired\t\texercise-dsp3-4\ttrue\ttrue\t\t\n",
                                    "3\trequired\t\texercise-dsp3-4\ttrue\tfalse\t\t\n")
        outputs = dict(self.with_layout(edited).outputs())
        self.assertNotIn(block_path, outputs)
        self.assertIn("  - design-process\n", outputs[block_path.parent / "field.yaml"])
        self.assertIn("\nopen: false\n", outputs[self.migration.blocks["vis-basic"].path()])

    def test_regression_links(self):
        blocks = Migration(self.root, regression="blocks")
        source = "dsp5.html#multiple-regression"
        self.assertEqual(self.migration.target(self.migration.blocks["causal-intro"], source),
                         "/lectures/dsp5.html#multiple-regression")
        self.assertEqual(blocks.target(blocks.blocks["causal-intro"], source),
                         "block:reg-multiple#partial-coefficients")
        for migration in (self.migration, blocks):
            source_block = migration.blocks["causal-intro"]
            self.assertEqual(migration.target(source_block, "dsp5.html"), "/lectures/dsp5.html")
            self.assertEqual(migration.target(source_block, "./dsp8.html"), "/lectures/dsp8.html")
            self.assertEqual(migration.target(source_block, "dsp4.html#correlation"), "block:num-correlation#correlation")

    def test_reviewed_requires_and_course(self):
        with (TABLES / "requires_review.tsv").open(encoding="utf-8", newline="") as table:
            reviewed = {row["block"]: row["proposed_requires"] for row in csv.DictReader(table, delimiter="\t")}
        reviewed["nlp-bert"] = "dl-transformer,nlp-processing,ml-classification"
        self.assertEqual({name: ",".join(block.requires) for name, block in self.migration.blocks.items()}, reviewed)
        verify_course(self.root, self.migration.chapters, self.migration.blocks, opened=False)
        course = (self.root / COURSE_YAML).read_text(encoding="utf-8")
        self.assertLess(course.index("block: survey-sem"), course.index("block: causal-intro"))
        self.assertIn("id: exercise-dsp10-2\n            file: dsp10-2.py", course)
        self.assertEqual(course.count("level: required"), 32)

    def test_material_content_id_and_links(self):
        for name in self.migration.materials:
            self.migration.verify_material(name, self.outputs[material_path(name)])
        scope = self.outputs[material_path("scope")]
        for old, new in [
            ("が中心です", "が重心です"),
            ("{#本科目で扱う範囲}", "{#removed-scope-id}"),
            ("(/lectures/setup.html)", "(setup.html)"),
        ]:
            with self.subTest(old=old), self.assertRaisesRegex(MigrationError, "構文木に差"):
                self.migration.verify_material("scope", self.replace_once(scope, old, new))

    def test_unlisted_material(self):
        path = self.root / "courses/dsp/stray.md"
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("unlisted material\n", encoding="utf-8")
        self.addCleanup(path.unlink)
        with self.assertRaisesRegex(MigrationError, "専用ページの表にないファイル"):
            dict(self.migration.outputs())

    def test_added_link_label_and_destination(self):
        for name, link in self.migration.added_links.items():
            with self.subTest(block=name, damaged="label"):
                self.verify_mutation(name, f"[{link.label}]", f"[{link.label}x]", "追加リンク")
            with self.subTest(block=name, damaged="target"):
                self.verify_mutation(name, link.target, "block:reg-multiple", "追加リンク")

    def test_course_yaml_rejects_invalid_layout(self):
        source = (self.root / COURSE_YAML).read_text(encoding="utf-8")
        block_order = "      - block: num-partial\n        level: advanced\n      - block: num-distance\n        level: required\n"
        cases = (
            ("level", "block: num-partial\n        level: advanced",
             "block: num-partial\n        level: required", "第 4 回の blocks"),
            ("order", block_order,
             "      - block: num-distance\n        level: required\n      - block: num-partial\n        level: advanced\n",
             "第 4 回の blocks"),
            ("duplicate guide", "\nopen: false\n", "\nopen: false\nmaterials: [guide]\n",
             "講義の materials"),
        )
        for case, old, new, diagnostic in cases:
            with self.subTest(case=case), tempfile.TemporaryDirectory() as folder:
                root = Path(folder)
                (root / COURSE_YAML).parent.mkdir()
                (root / COURSE_YAML).write_text(self.replace_once(source, old, new), encoding="utf-8")
                (root / "pages").symlink_to(self.root / "pages", target_is_directory=True)
                with self.assertRaisesRegex(MigrationError, diagnostic):
                    verify_course(root, self.migration.chapters, self.migration.blocks, opened=False)

    def test_reviewed_exercises_warning_and_kind(self):
        case = self.migration.blocks["dl-training-case"]
        procedure = self.migration.blocks["dl-training-procedure"]
        self.assertEqual(case.exercises, ("exercise-dsp10-2", "exercise-dsp10-3"))
        self.assertEqual(procedure.exercises, ())
        warning = procedure.chapter.body[procedure.warning.start:procedure.warning.end]
        draft = self.outputs[procedure.path()]
        self.assertIn(warning, draft)
        with self.assertRaisesRegex(MigrationError, "warn が原稿と逐語一致"):
            self.migration.verify_body(procedure, draft.replace(warning, "", 1))
        with self.assertRaisesRegex(MigrationError, "構文木に差"):
            self.migration.verify_body(procedure, draft.replace(warning, warning + "\n" + warning, 1))
        self.assertIn("\nkind: code\n", self.outputs[Path("fields/bayes/bayes-setup.md")])
        self.assertEqual(self.migration.blocks["vis-more"].anchors["クラスタリングにおける散布図"], "scatter-3d")

    def test_chapter_link_inventory(self):
        inventory = self.root / "chapter-links.tsv"
        self.migration.write_chapter_links(inventory)
        with inventory.open(encoding="utf-8", newline="") as source:
            entries = list(csv.DictReader(source, delimiter="\t"))
        self.assertTrue(any(row["source_chapter"] == "1" and not row["blocks"] for row in entries))
        self.assertTrue(any(row["blocks"] and row["old_target"] == "dsp8.html" for row in entries))
        self.assertTrue(all(row["target"].startswith("/lectures/dsp") and "#" not in row["target"] for row in entries))
        expected = Counter((name, row["target"]) for row in entries for name in filter(None, row["blocks"].split(",")))
        actual = Counter()
        for block in self.migration.blocks.values():
            _, body = front_matter(self.outputs[block.path()])
            for node in nodes(parse(body)):
                if node["t"] == "Link":
                    urls = [node["c"][2][0]]
                elif raw_content(node):
                    urls = [url for _, _, attr, url in RawTags(node["c"][1]).urls if attr == "href"]
                else:
                    continue
                for url in urls:
                    parts = urlsplit(url)
                    if not (parts.scheme or parts.netloc or parts.fragment) and lecture_chapter(parts.path):
                        actual[(block.name, url)] += 1
        self.assertEqual(actual, expected)

    def test_html_detects_relocated_id_and_changed_image(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            old = root / "old.html"
            old.write_text('<article><h2 id="a">One</h2><span id="specific"></span>'
                           '<img src="/original.png"><h2 id="b">Two</h2></article>', encoding="utf-8")
            _, (_, first), (_, second) = sections(article_body(old))
            path = root / "fields/field/block/index.html"
            path.parent.mkdir(parents=True)
            path.write_text('<article><h2 id="a">One</h2><img src="/changed.png">'
                            '<h2 id="b">Two</h2><span id="specific"></span></article>', encoding="utf-8")
            issues = []
            pages = {"index": [Section("a", "a", first), Section("b", "b", second)]}
            compare_block(root, 3, "field", "block", pages, issues, defaultdict(list), LinkTargets([]))
            self.assertTrue(any("dsp3#specific" in issue and "#a" in issue for issue in issues), issues)
            self.assertTrue(any("画像の参照先" in issue for issue in issues), issues)

    def test_html_detects_href_to_another_existing_anchor(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            old_path = root / "lectures/dsp3.html"
            old_path.parent.mkdir(parents=True)
            old_path.write_text('<article><h2 id="a">One</h2><a href="#b">Read</a>'
                                '<h2 id="b">Two</h2><span id="other"></span></article>', encoding="utf-8")
            _, (_, first), (_, second) = sections(article_body(old_path))
            pages = {"index": [Section("a", "a", first)], "two": [Section("b", "two", second)]}
            directory = root / "fields/field/block"
            directory.mkdir(parents=True)
            (directory / "two.html").write_text('<article><h1 id="two"><span id="b"></span>Two</h1>'
                                                '<span id="other"></span></article>', encoding="utf-8")
            index = directory / "index.html"
            index.write_text('<article><h2 id="a">One</h2><a href="two.html#other">Read</a></article>', encoding="utf-8")
            self.assertIn('id="other"', (directory / "two.html").read_text(encoding="utf-8"))
            issues = []
            expected = LinkTargets([(3, "field", "block", pages)])
            compare_block(root, 3, "field", "block", pages, issues, defaultdict(list), expected)
            self.assertEqual(len(issues), 1, issues)
            self.assertRegex(issues[0], "href 1 件目.*two\\.html#b.*two\\.html#other")

    def test_html_regression_target_comes_from_the_legacy_build(self):
        with tempfile.TemporaryDirectory() as folder:
            base = Path(folder)
            regression = base / "fields/regression/reg-multiple"
            regression.mkdir(parents=True)
            (regression / "coefficients.html").write_text('<span id="partial-coefficients"></span>', encoding="utf-8")
            mapping = base / "reg_old_anchors.tsv"
            mapping.write_text("old_id\tblock\tblock_id\nmultiple-regression\treg-multiple\tpartial-coefficients\n",
                               encoding="utf-8")
            route = Path("lectures/dsp4.html")
            source = "dsp5.html#multiple-regression"
            self.assertEqual(LinkTargets([]).href(route, "block", source), "/lectures/dsp5.html#multiple-regression")
            links = LinkTargets([], regression_links(base, mapping))
            self.assertEqual(links.href(route, "block", source), "/fields/regression/reg-multiple/coefficients.html#partial-coefficients")
            self.assertEqual(links.href(route, "block", "dsp5.html"), "/lectures/dsp5.html")


if __name__ == "__main__":
    unittest.main(verbosity=2)
