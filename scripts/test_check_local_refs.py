import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch
from urllib.parse import quote

from scripts.check_local_refs import BrokenRef, broken_refs


class CheckLocalRefsTest(unittest.TestCase):
    def setUp(self) -> None:
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        self.root = Path(temp.name).resolve()

    def write_page(self, name: str, html: str) -> None:
        path = self.root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(html, encoding="utf-8")

    def test_same_page_fragment_is_case_sensitive(self) -> None:
        self.write_page(
            "index.html",
            '<section id="Section"></section><a href="#Section">OK</a>'
            '<a href="#section">Broken</a>',
        )
        self.assertEqual(
            broken_refs(self.root), [BrokenRef(Path("index.html"), "#section", True)]
        )

    def test_cross_page_fragments_and_cache(self) -> None:
        self.write_page(
            "index.html",
            '<a href="part/index.html#present">OK</a>'
            '<a href="part/index.html#missing">Broken</a>'
            '<a href="/part/#present">OK</a>'
            '<a href="part/../part/index.html#present">OK</a>',
        )
        self.write_page(
            "part/index.html",
            '<div id="present"></div><a href="../index.html">Back</a>',
        )
        with patch.object(Path, "read_text", autospec=True, side_effect=Path.read_text) as read:
            broken = broken_refs(self.root)
        self.assertEqual(
            broken, [BrokenRef(Path("index.html"), "part/index.html#missing", True)]
        )
        self.assertCountEqual(
            [call.args[0] for call in read.call_args_list],
            [self.root / "index.html", self.root / "part/index.html"],
        )

    def test_encoded_fragment_and_named_anchor(self) -> None:
        self.write_page(
            "index.html",
            f'<a href="#{quote("見出し")}">OK</a><h2 id="見出し"></h2>'
            '<a name="legacy"></a><a href="#legacy">OK</a>'
            '<input name="input-name"><a href="#input-name">Broken</a>',
        )
        self.assertEqual(
            broken_refs(self.root), [BrokenRef(Path("index.html"), "#input-name", True)]
        )

    def test_ignored_fragments(self) -> None:
        self.write_page("chapter.html", "<h2>Chapter</h2>")
        self.write_page(
            "index.html",
            '<a href="#">OK</a><a href="chapter.html#">OK</a>'
            '<a href="https://example.org/chapter.html#missing">External</a>'
            '<a href="//example.org/chapter.html#missing">External</a>'
            '<img src="chapter.html#missing"><link href="chapter.html#missing">'
            '<script src="chapter.html#missing"></script>',
        )
        self.assertEqual(broken_refs(self.root), [])

    def test_cli_lists_both_kinds_of_broken_reference(self) -> None:
        self.write_page(
            "index.html",
            '<a href="missing.html#section">Missing file</a>'
            '<img src="missing.svg#shape">'
            '<a href="#section">Missing ID</a>',
        )
        script = Path(__file__).with_name("check_local_refs.py")
        result = subprocess.run(
            [sys.executable, str(script), str(self.root)], capture_output=True, text=True
        )
        self.assertEqual(result.returncode, 1)
        self.assertEqual(
            result.stdout,
            "index.html\tmissing.html#section\nindex.html\tmissing.svg#shape\nindex.html\t#section\n",
        )
        self.assertEqual(
            result.stderr, "check_local_refs: 参照切れ 3 件 (file なし 2 件, ID なし 1 件)\n"
        )
