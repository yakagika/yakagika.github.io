#!/usr/bin/env python3
"""Generate the probability basics diagrams and their double-resolution PNGs."""

from dataclasses import dataclass
from html import escape
from itertools import product
from pathlib import Path
import re
from subprocess import run

ROOT = Path(__file__).resolve().parents[1] / "images/fields/probability/prob-basics"
WIDTH = 960
INK = "#22324a"
GRAY = "#66758b"
GUIDE = "#c3cdd9"
BLUE = "#225f99"
RED = "#ac482b"
MATH_FONT = "Times, 'Times New Roman', serif"
BLUE_FILL = "#dceaf6"
RED_FILL = "#f8e3da"
OVERLAP_FILL = "#e5e0f1"


class Diagram:
    def __init__(self):
        self.elements = []
        self.clip_paths = []

    def rect(self, x, y, w, h, fill="none", stroke=INK, width=2):
        self.elements.append(
            f'<rect x="{x}" y="{y}" width="{w}" height="{h}" fill="{fill}" '
            f'stroke="{stroke}" stroke-width="{width}"/>'
        )

    def line(self, x1, y1, x2, y2, stroke=GUIDE, width=2):
        self.elements.append(
            f'<line x1="{x1}" y1="{y1}" x2="{x2}" y2="{y2}" '
            f'stroke="{stroke}" stroke-width="{width}"/>'
        )

    def label(self, x, y, value, *, size=30, math=False, anchor="middle", color=INK):
        content = escape(value)
        if math:
            content = re.sub(r"([ABC])", r'<tspan font-style="italic">\1</tspan>', content)
            content = content.replace(
                "ᶜ",
                '<tspan font-style="italic" font-size="84%" baseline-shift="super">c</tspan>',
            )
        font = f' font-family="{MATH_FONT}"' if math else ""
        self.elements.append(
            f'<text x="{x}" y="{y}" fill="{color}" font-size="{size}" '
            f'text-anchor="{anchor}"{font}>{content}</text>'
        )

    def region(self, box, clips, fill):
        x, y, w, h = box
        for clip in clips:
            self.elements.append(f'<g clip-path="url(#{clip})">')
        self.rect(x, y, w, h, fill=fill, stroke="none")
        self.elements.extend("</g>" for _ in clips)

    def clip(self, shape):
        name = f"clip-{len(self.clip_paths) + 1}"
        self.clip_paths.append(f'<clipPath id="{name}">{shape}</clipPath>')
        return name

    def write(self, name, height, title):
        svg = ROOT / f"{name}.svg"
        definitions = "\n    ".join(self.clip_paths)
        body = "\n    ".join(self.elements)
        svg.write_text(
            f'<svg xmlns="http://www.w3.org/2000/svg" width="{WIDTH}" '
            f'height="{height}" viewBox="0 0 {WIDTH} {height}" '
            'role="img" aria-labelledby="title">\n'
            f'  <title id="title">{escape(title)}</title>\n'
            f"  <defs>\n    {definitions}\n  </defs>\n"
            f'  <rect width="{WIDTH}" height="{height}" fill="#fff"/>\n'
            '  <g font-family="Hiragino Sans, Noto Sans CJK JP, Arial, sans-serif">\n'
            f"    {body}\n"
            "  </g>\n"
            "</svg>\n",
            encoding="utf-8",
        )
        run(
            ["rsvg-convert", "-w", str(2 * WIDTH), "-o", str(svg.with_suffix(".png")),
             str(svg)],
            check=True,
        )


@dataclass(frozen=True)
class Circle:
    x: int
    y: int
    r: int

    def svg(self, fill="none", stroke=INK):
        return (
            f'<circle cx="{self.x}" cy="{self.y}" r="{self.r}" '
            f'fill="{fill}" stroke="{stroke}" stroke-width="2"/>'
        )

    def outside(self, box):
        x, y, w, h = box
        left, right = self.x - self.r, self.x + self.r
        # The even-odd rule cuts a circle out of the surrounding sample space.
        return (
            f'<path d="M {x} {y} h {w} v {h} h {-w} Z '
            f'M {left} {self.y} A {self.r} {self.r} 0 1 0 {right} {self.y} '
            f'A {self.r} {self.r} 0 1 0 {left} {self.y} Z" clip-rule="evenodd"/>'
        )


def venn(diagram, box, circles, fill_for, labels):
    # Color each mutually exclusive region according to its event memberships.
    clips = [
        (diagram.clip(circle.outside(box)), diagram.clip(circle.svg(fill=INK, stroke="none")))
        for circle in circles
    ]
    for membership in product((False, True), repeat=len(circles)):
        fill = fill_for(*membership)
        if fill:
            diagram.region(box, [pair[inside] for inside, pair in zip(membership, clips)], fill)

    diagram.rect(*box)
    diagram.elements.extend(circle.svg() for circle in circles)
    x, y, _, _ = box
    diagram.label(x + 12, y + 29, "Ω", size=26, math=True, anchor="start")
    for lx, ly, label in labels:
        diagram.label(lx, ly, label, math=True)


def complement():
    diagram = Diagram()
    for x, inside, label, fill, color in (
        (20, True, "A", BLUE_FILL, BLUE),
        (500, False, "Aᶜ", RED_FILL, RED),
    ):
        venn(
            diagram, (x, 14, 440, 204), (Circle(x + 210, 116, 78),),
            lambda a: fill if a == inside else None,
            ((x + 210, 126, "A"), (x + 340, 126, "Aᶜ")),
        )
        diagram.label(x + 220, 257, label, math=True, size=34, color=color)
    diagram.write("complement", 276, "標本空間における事象と補事象")


def union_intersection():
    diagram = Diagram()
    for x, label, selected in (
        (20, "A ∪ B", lambda a, b: a or b),
        (500, "A ∩ B", lambda a, b: a and b),
    ):
        venn(
            diagram, (x, 14, 440, 214),
            (Circle(x + 168, 121, 84), Circle(x + 272, 121, 84)),
            lambda a, b: BLUE_FILL if selected(a, b) else None,
            ((x + 141, 131, "A"), (x + 299, 131, "B")),
        )
        diagram.label(x + 220, 268, label, math=True, size=34)
    diagram.write("union-intersection", 286, "和事象と積事象の領域")


def event_relationships():
    diagram = Diagram()
    for x, circles, label, positions in (
        (12, (Circle(141, 119, 41), Circle(162, 119, 76)), "A ⊆ B", (141, 206)),
        (330, (Circle(439, 119, 66), Circle(521, 119, 66)), "A ∩ B ≠ ∅", (421, 539)),
        (648, (Circle(731, 119, 54), Circle(865, 119, 54)), "A ∩ B = ∅", (731, 865)),
    ):
        venn(
            diagram, (x, 14, 300, 210), circles,
            lambda a, b: OVERLAP_FILL if a and b else None,
            ((positions[0], 129, "A"), (positions[1], 129, "B")),
        )
        diagram.label(x + 150, 263, label, math=True, size=34)
    diagram.write("event-relationships", 282, "包含, 重なり, 排反のベン図")


def set_laws():
    diagram = Diagram()
    equations = (
        ("(A ∪ B) ∩ C", "(A ∩ C) ∪ (B ∩ C)",
         lambda a, b, c: (a or b) and c, lambda a, b, c: (a and c) or (b and c)),
        ("(A ∩ B) ∪ C", "(A ∪ C) ∩ (B ∪ C)",
         lambda a, b, c: (a and b) or c, lambda a, b, c: (a or c) and (b or c)),
        ("(A ∪ B)ᶜ", "Aᶜ ∩ Bᶜ", lambda a, b: not (a or b), lambda a, b: not a and not b),
        ("(A ∩ B)ᶜ", "Aᶜ ∪ Bᶜ", lambda a, b: not (a and b), lambda a, b: not a or not b),
    )

    for (x, y), (left, right, lhs, rhs) in zip(
        ((12, 12), (492, 12), (12, 238), (492, 238)), equations
    ):
        three_events = y == 12
        for offset, label, selected in ((0, left, lhs), (248, right, rhs)):
            panel_x = x + offset
            cy = y + (112 if three_events else 84)
            circles = (
                Circle(panel_x + 73, cy, 49),
                Circle(panel_x + 135, cy, 49),
            )
            labels = ((panel_x + 58, cy + 13, "A"), (panel_x + 150, cy + 13, "B"))
            if three_events:
                circles += (Circle(panel_x + 104, cy - 45, 49),)
                labels += ((panel_x + 104, cy - 50, "C"),)
            venn(
                diagram, (panel_x, y, 208, 174), circles,
                lambda *members: BLUE_FILL if selected(*members) else None,
                labels,
            )
            diagram.label(panel_x + 104, y + 207, label, math=True, size=27)
        diagram.label(x + 228, y + 99, "=", math=True, size=32)

    diagram.line(480, 12, 480, 446)
    diagram.write("set-laws", 460, "分配法則とド・モルガンの法則の両辺")


def addition_rule():
    diagram = Diagram()
    venn(
        diagram, (16, 52, 420, 234), (Circle(117, 169, 76), Circle(335, 169, 76)),
        lambda a, b: BLUE_FILL if a else RED_FILL if b else None,
        ((117, 179, "A"), (335, 179, "B")),
    )
    fills = {(True, False): BLUE_FILL, (True, True): OVERLAP_FILL, (False, True): RED_FILL}
    venn(
        diagram, (464, 52, 480, 234), (Circle(640, 169, 106), Circle(768, 169, 106)),
        lambda a, b: fills.get((a, b)),
        ((600, 140, "A"), (808, 140, "B")),
    )
    for x, label in ((600, "A ∩ Bᶜ"), (704, "A ∩ B"), (808, "Aᶜ ∩ B")):
        diagram.label(x, 185, label, math=True, size=26)
    diagram.label(226, 34, "排反の場合")
    diagram.label(704, 34, "共通部分がある場合")
    diagram.write("addition-rule", 301, "排反の場合と共通部分がある場合の和事象")


def conditional_balls():
    diagram = Diagram()
    white_balls = ((211, 103, 1), (183, 209, 1), (292, 257, 2))
    red_balls = ((117, 116, 2), (107, 247, 2), (320, 159, 1))
    for x, label in ((0, "すべての玉"), (520, "白玉に限った場合")):
        diagram.label(x + 220, 35, label)
        diagram.rect(x + 52, 53, 336, 278)
        for bx, by, number in white_balls:
            diagram.elements.append(Circle(x + bx, by, 32).svg(fill="#fff"))
            diagram.label(x + bx, by + 12, str(number), size=35)
    for x, y, number in red_balls:
        diagram.elements.append(Circle(x, y, 32).svg(fill=RED_FILL))
        diagram.label(x, y + 12, str(number), size=35)
    for balls, color_name in ((white_balls, "白"), (red_balls, "赤")):
        for x, y, _ in balls:
            diagram.label(x, y + 57, color_name, size=24)
    diagram.line(440, 178, 520, 178, stroke=GRAY, width=3)
    diagram.elements.append(f'<path d="M 505 168 L 520 178 L 505 188" fill="none" '
                            f'stroke="{GRAY}" stroke-width="3"/>')
    diagram.write("conditional-balls", 346, "白玉を条件にしたときに残る玉")


def coin_outcomes():
    diagram = Diagram()
    diagram.label(480, 41, "裏 = 0　表 = 1", size=34)
    diagram.rect(86, 61, 788, 339)
    diagram.label(109, 105, "Ω", math=True, size=34)
    diagram.rect(178, 76, 312, 306, fill=BLUE_FILL, stroke="none")
    diagram.rect(144, 131, 660, 72, fill=RED_FILL, stroke="none")
    diagram.rect(178, 131, 312, 72, fill=OVERLAP_FILL, stroke="none")
    diagram.rect(178, 76, 312, 306)
    diagram.rect(144, 131, 660, 72)
    diagram.label(208, 115, "A", math=True, size=36)
    diagram.label(777, 181, "B", math=True, size=36)
    diagram.label(401, 181, "A ∩ B", math=True, size=34)
    for x, outcomes in ((266, ("111", "110", "101", "100")),
                        (644, ("000", "001", "010", "011"))):
        for y, outcome in zip((180, 241, 296, 351), outcomes):
            diagram.label(x, y, outcome, size=40)
    diagram.write("coin-outcomes", 414, "コインを 3 回投げるときの事象と共通部分")


def main():
    ROOT.mkdir(parents=True, exist_ok=True)
    complement()
    union_intersection()
    event_relationships()
    set_laws()
    addition_rule()
    conditional_balls()
    coin_outcomes()


if __name__ == "__main__":
    main()
