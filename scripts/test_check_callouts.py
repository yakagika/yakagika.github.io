"""scripts/check_callouts.py の囲みの数え方の test."""
from __future__ import annotations

import unittest

from scripts.check_callouts import count_callouts, is_target


class CountCalloutsTest(unittest.TestCase):
    def test_counts_note_and_warn_forms(self) -> None:
        text = "\n".join([
            "::: note", "補足", ":::",
            ":::warn", "注意", ":::",
            "::: {.note}", "補足", ":::",
            "::::: warn", "注意", ":::::",
        ])
        self.assertEqual(count_callouts(text), 4)

    def test_ignores_other_divs_and_closing_lines(self) -> None:
        text = "\n".join(["::: {#refs}", ":::", "::: choice-tree", ":::"])
        self.assertEqual(count_callouts(text), 0)

    def test_ignores_code_blocks(self) -> None:
        text = "\n".join(["```markdown", "::: note", ":::", "```"])
        self.assertEqual(count_callouts(text), 0)

    def test_targets_only_lecture_markdown(self) -> None:
        self.assertTrue(is_target("courses/dsproj/plan.md"))
        self.assertTrue(is_target("fields/regression/reg-intro.md"))
        self.assertFalse(is_target("courses/dsproj.yaml"))
        self.assertFalse(is_target("posts/2026-01-01-x.md"))


if __name__ == "__main__":
    unittest.main()
