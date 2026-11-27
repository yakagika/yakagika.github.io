#!/usr/bin/env python3
"""Generate sampling distribution figures and double-resolution PNGs."""

import re
from html import escape
from math import atan, cos, exp, lgamma, log, log1p, pi, sqrt, tau
from pathlib import Path
from statistics import NormalDist
from subprocess import run

IMAGE_ROOT = Path(__file__).resolve().parents[1] / "images/fields"
OUTPUT = IMAGE_ROOT / "probability" / "prob-sampling"
WIDTH = 960
INK = "#22324a"
GUIDE = "#c3cdd9"
BLUE = "#225f99"
RED = "#ac482b"
MATH_FONT = "Times, 'Times New Roman', serif"
BLUE_FILL = "#dceaf6"
RED_FILL = "#f8e3da"
SANS_FONT = "Hiragino Sans, Noto Sans CJK JP, Arial, sans-serif"

class Diagram:
    def __init__(self):
        self.elements = []

    def rect(self, x, y, width, height, fill="none"):
        self.elements.append(
            f'<rect x="{x:.2f}" y="{y:.2f}" width="{width:.2f}" height="{height:.2f}" '
            f'fill="{fill}" stroke="{INK}" stroke-width="2"/>'
        )

    def line(self, x1, y1, x2, y2, *, stroke=INK, dash=None, arrow=False):
        attrs = f' stroke-dasharray="{dash}"' if dash else ""
        if arrow:
            attrs += ' marker-end="url(#arrow)"'
        self.elements.append(
            f'<line x1="{x1:.2f}" y1="{y1:.2f}" x2="{x2:.2f}" y2="{y2:.2f}" '
            f'stroke="{stroke}" stroke-width="2"{attrs}/>'
        )

    def label(self, x, y, value, *, size=24, math=False, anchor="middle", fill=INK):
        font = f' font-family="{MATH_FONT}"' if math else ""
        parts = re.split(r"(χ²_\{[0-9mnν]+\}|X̄ₙ|Xₙ|xₙ|θ̂|Ga|Bi|Ex|[abfknptux-zNXYZSαμφσλ])",
                         value) if math else re.split(r"(X̄ₙ|Xₙ|xₙ|θ̂)", value)
        content = []
        for part in parts:
            if part.startswith("χ²_{"):
                degree = escape(part[4:-1])
                style = "" if degree.isdecimal() else ' font-style="italic"'
                content.append(
                    f'<tspan font-style="italic">χ</tspan>²'
                    f'<tspan dy="5" font-size="80%"{style}>{degree}</tspan>'
                    '<tspan dy="-5">&#8203;</tspan>'
                )
            elif part in ("X̄ₙ", "Xₙ", "xₙ"):
                bar = ' text-decoration="overline"' if part == "X̄ₙ" else ""
                content.append(f'<tspan font-style="italic"{bar}>{part[0]}</tspan>'
                               '<tspan font-style="italic" dy="5" font-size="80%">n</tspan>'
                               '<tspan dy="-5">&#8203;</tspan>')
            elif part == "θ̂":
                content.append(
                    '<tspan font-style="italic">θ</tspan>'
                    '<tspan dx="-10" dy="-4">ˆ</tspan>'
                    '<tspan dx="2" dy="4">&#8203;</tspan>'
                )
            elif math and re.fullmatch(r"[abfknptux-zNXYZSαμφσλ]", part):
                content.append(f'<tspan font-style="italic">{part}</tspan>')
            else:
                content.append(escape(part))
        self.elements.append(
            f'<text x="{x:.2f}" y="{y:.2f}" fill="{fill}" font-size="{size}" '
            f'text-anchor="{anchor}"{font}>{"".join(content)}</text>'
        )

    def path(self, points, *, stroke=INK, width=3, fill="none", dash=None):
        coords = " L ".join(f"{x:.2f} {y:.2f}" for x, y in points)
        close = " Z" if fill != "none" else ""
        pattern = f' stroke-dasharray="{dash}"' if dash else ""
        self.elements.append(
            f'<path d="M {coords}{close}" fill="{fill}" stroke="{stroke}" '
            f'stroke-width="{width}" stroke-linejoin="round"{pattern}/>'
        )

    def dot(self, x, y, *, radius=4, fill=INK):
        self.elements.append(
            f'<circle cx="{x:.2f}" cy="{y:.2f}" r="{radius}" fill="{fill}" '
            f'stroke="{INK}" stroke-width="2"/>'
        )

    def box(self, x, y, width, height, title, detail=None, *, fill=BLUE_FILL):
        self.rect(x, y, width, height, fill)
        cx = x + width / 2
        if detail is None:
            self.label(cx, y + height / 2 + 8, title)
        else:
            self.label(cx, y + height / 2 - 7, title)
            self.label(cx, y + height / 2 + 24, detail, math=True)

    def clip(self, name, x, y, width, height):
        self.elements.append(
            f'<clipPath id="{name}"><rect x="{x}" y="{y}" '
            f'width="{width}" height="{height}"/></clipPath>'
            f'<g clip-path="url(#{name})">'
        )

    def end_clip(self):
        self.elements.append("</g>")

    def write(self, folder, name, height, title):
        folder.mkdir(parents=True, exist_ok=True)
        svg = folder / f"{name}.svg"
        body = "\n    ".join(self.elements)
        svg.write_text(
            f'<svg xmlns="http://www.w3.org/2000/svg" width="{WIDTH}" '
            f'height="{height}" viewBox="0 0 {WIDTH} {height}" '
            'role="img" aria-labelledby="title">\n'
            f'  <title id="title">{escape(title)}</title>\n'
            '  <defs>\n'
            '    <marker id="arrow" viewBox="0 0 10 10" refX="10" refY="5" '
            'markerWidth="5" markerHeight="5" orient="auto">\n'
            f'      <path d="M 0 0 L 10 5 L 0 10 Z" fill="{INK}"/>\n'
            '    </marker>\n'
            '  </defs>\n'
            f'  <rect width="{WIDTH}" height="{height}" fill="#fff"/>\n'
            f'  <g font-family="{SANS_FONT}">\n'
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


def axes(diagram, left, top, right, bottom):
    diagram.line(left, bottom, left, top, arrow=True)
    diagram.line(left, bottom, right, bottom, arrow=True)


def x_tick(diagram, x, bottom, label, *, anchor="middle"):
    diagram.line(x, bottom, x, bottom + 7)
    diagram.label(x, bottom + 32, label, math=True, anchor=anchor)


def y_tick(diagram, left, y, label):
    diagram.line(left - 7, y, left, y)
    diagram.label(left - 13, y + 8, label, math=True, anchor="end")


def normal_density(value, mean, deviation):
    return exp(-0.5 * ((value - mean) / deviation) ** 2) / (deviation * sqrt(tau))


class Plot:
    def __init__(self, diagram, box, domain, peak):
        self.diagram = diagram
        self.left, self.top, self.right, self.bottom = box
        self.low, self.high = domain
        self.peak = peak

    def x(self, value):
        return self.left + (self.right - self.left) * (value - self.low) / (
            self.high - self.low
        )

    def y(self, value):
        return self.bottom - (self.bottom - self.top) * value / self.peak

    def curve(self, density, *, stroke=INK, dash=None):
        points = self.sample(density, self.low, self.high)
        self.diagram.path(points, stroke=stroke, dash=dash)

    def sample(self, density, low, high):
        values = [low + (high - low) * i / 240 for i in range(241)]
        return [(self.x(value), self.y(density(value))) for value in values]

    def area(self, density, low, high, fill=BLUE_FILL):
        points = [(self.x(low), self.bottom), *self.sample(density, low, high),
                  (self.x(high), self.bottom)]
        self.diagram.path(points, width=1.5, fill=fill)

    def guide(self, value, height):
        self.diagram.line(self.x(value), self.bottom, self.x(value), self.y(height),
                          stroke=INK, dash="5 5")

    def frame(self, x_label, y_label, ticks=(), y_ticks=()):
        d = self.diagram
        axes(d, self.left, self.top - 8, self.right + 15, self.bottom)
        d.label(self.left, self.top - 20, y_label, anchor="start",
                math=y_label.isascii() or y_label == "φ(z)")
        if x_label:
            d.label(self.right + 25, self.bottom + 8, x_label, anchor="start",
                    math=True)
        for tick in ticks:
            value, label = tick if isinstance(tick, tuple) else (tick, number(tick))
            x_tick(d, self.x(value), self.bottom, label)
        for tick in y_ticks:
            value, label = tick if isinstance(tick, tuple) else (tick, number(tick))
            y_tick(d, self.left, self.y(value), label)


def number(value):
    return str(value).replace("-", "−")


def gaussian(mean, deviation):
    return lambda value: normal_density(value, mean, deviation)


def chi_density(x, df):
    if x == 0 and df >= 2:
        return 0.5 if df == 2 else 0.0
    shape = df / 2
    return exp((shape - 1) * log(x) - x / 2 - shape * log(2) - lgamma(shape))


def t_density(x, df):
    log_scale = lgamma((df + 1) / 2) - lgamma(df / 2) - log(df * pi) / 2
    return exp(log_scale - (df + 1) / 2 * log1p(x * x / df))


def f_density(x, numerator, denominator):
    if x == 0:
        return 1.0 if numerator == 2 else 0.0
    a, b = numerator / 2, denominator / 2
    scale = numerator / denominator
    return exp(
        lgamma(a + b) - lgamma(a) - lgamma(b) + a * log(scale)
        + (a - 1) * log(x) - (a + b) * log1p(scale * x)
    )


def t_upper_point(df, probability):
    # x = sqrt(df) * tan(angle) maps the infinite tail to a bounded integral.
    def tail(x):
        low = atan(x / sqrt(df))
        steps = 512
        step = (pi / 2 - low) / steps
        values = [cos(low + k * step) ** (df - 1) for k in range(steps + 1)]
        integral = step / 3 * (
            values[0] + values[-1] + 4 * sum(values[1:-1:2])
            + 2 * sum(values[2:-1:2])
        )
        scale = exp(lgamma((df + 1) / 2) - lgamma(df / 2)) / sqrt(pi)
        return scale * integral

    low, high = 0.0, 1.0
    while tail(high) > probability:
        high *= 2
    for _ in range(48):
        mid = (low + high) / 2
        if tail(mid) > probability:
            low = mid
        else:
            high = mid
    return (low + high) / 2


STYLES = ((INK, None), (BLUE, "14 6"), (RED, "3 6"),
          (BLUE, "17 5 3 5"), (INK, "6 5"))


def curve_key(diagram, x, y, name, style):
    stroke, dash = style
    diagram.line(x, y - 7, x + 36, y - 7, stroke=stroke, dash=dash)
    diagram.label(x + 46, y, name, anchor="start", math=True)


def chi_square():
    d = Diagram()
    p = Plot(d, (118, 65, 750, 335), (0, 16), 0.55)
    d.clip("density", p.left, p.top, p.right - p.left, p.bottom - p.top)
    for df, (stroke, dash) in zip((1, 2, 3, 5, 10), STYLES):
        low = 0.001 if df == 1 else 0
        d.path(p.sample(lambda x: chi_density(x, df), low, p.high),
               stroke=stroke, dash=dash)
    d.end_clip()
    p.frame("x", "確率密度", range(0, 17, 2), (0, 0.1, 0.2, 0.3, 0.4, 0.5))
    d.line(p.x(0.37), 96, p.x(0.37), p.top + 3, arrow=True)
    d.label(176, 84, "表示上限 0.55", anchor="start", size=22)
    for i, (df, style) in enumerate(zip((1, 2, 3, 5, 10), STYLES)):
        curve_key(d, 790, 98 + i * 45, f"χ²_{{{df}}}", style)
    d.write(OUTPUT, "chi-square-densities", 390, "カイ二乗分布の確率密度")


def student_t():
    d = Diagram()
    dfs = (None, 1, 2, 10, 30)
    names = ("N(0,1)", "t(1)", "t(2)", "t(10)", "t(30)")
    densities = (gaussian(0, 1),) + tuple(
        lambda x, df=df: t_density(x, df) for df in dfs[1:]
    )
    probabilities = tuple(
        NormalDist().inv_cdf(0.975) if df is None else t_upper_point(df, 0.025)
        for df in dfs
    )

    main = Plot(d, (104, 61, 740, 248), (-5, 5), 0.42)
    main.frame("x", "確率密度", range(-4, 5, 2), (0, 0.1, 0.2, 0.3, 0.4))
    for density, (stroke, dash) in zip(densities, STYLES):
        main.curve(density, stroke=stroke, dash=dash)
    for i, (name, style) in enumerate(zip(names, STYLES)):
        curve_key(d, 790, 86 + i * 34, name, style)

    tail = Plot(d, (104, 324, 740, 521), (1.5, 14), 0.045)
    d.clip("tail", tail.left, tail.top, tail.right - tail.left,
           tail.bottom - tail.top)
    for density, (stroke, dash) in zip(densities, STYLES):
        tail.curve(density, stroke=stroke, dash=dash)
    d.end_clip()
    tail.frame("x", "右裾の確率密度 (表示上限 0.045)", (4, 6, 8, 10, 12, 14),
               (0, 0.01, 0.02, 0.03, 0.04))

    # Stagger the three labels near x = 2, starting with the rightmost one.
    label_offsets = ((18, 117), (18, 61), (46, 61), (18, 61), (18, 89))
    d.label(790, tail.top + 14, "上側 2.5% 点", anchor="start", size=22)
    for name, point, (dx, dy), (stroke, _) in zip(
        names, probabilities, label_offsets, STYLES
    ):
        px = tail.x(point)
        d.line(px, tail.bottom - 8, px, tail.bottom + 10, stroke=stroke)
        label_y = tail.bottom + dy
        d.path([(px, tail.bottom + 10), (px, tail.bottom + 42),
                (px + dx - 5, label_y - 7)], stroke=stroke, width=1.5)
        d.label(px + dx, label_y, f"{name}  {point:.3f}",
                size=22, anchor="start", math=True)
    d.write(OUTPUT, "student-t-densities-and-tails", 659,
            "t 分布と標準正規分布の密度, 右裾と上側 2.5% 点")


def fisher_f():
    d = Diagram()
    p = Plot(d, (110, 65, 726, 340), (0, 4.5), 1.08)
    p.frame("x", "確率密度", (0, 1, 2, 3, 4), (0, 0.25, 0.5, 0.75, 1))
    d.line(p.x(1), 102, p.x(1), p.bottom, stroke=GUIDE, dash="5 5")
    for params, style in zip(((2, 10), (5, 10), (10, 10), (10, 50)), STYLES):
        stroke, dash = style
        p.curve(lambda x: f_density(x, *params), stroke=stroke, dash=dash)
    for i, (name, style) in enumerate(
        zip(("F(2,10)", "F(5,10)", "F(10,10)", "F(10,50)"), STYLES)
    ):
        curve_key(d, 763, 104 + i * 48, name, style)
    d.write(OUTPUT, "f-densities", 390, "自由度の組を変えた F 分布の確率密度")


def distribution_relationships():
    d = Diagram()
    left, middle, right = 25, 370, 715
    width, height = 220, 55
    for x, y, title in (
        (left, 22, "N(μ, σ²)"), (middle, 22, "Z ∼ N(0,1)"),
        (right, 22, "Z² ∼ χ²_{1}"), (left, 149, "Ex(1/2)"),
        (middle, 149, "Ga(n/2, 1/2)"), (right, 149, "χ²_{n}"),
    ):
        d.rect(x, y, width, height, BLUE_FILL)
        d.label(x + width / 2, y + 36, title, math=True)
    for x, y, label in (
        (left, 50, "標準化"), (middle, 50, "二乗"),
        (left, 176, "独立な和¹"), (middle, 176, "同じ分布"),
    ):
        d.line(x + width + 4, y, x + 341, y, arrow=label != "同じ分布")
        d.label(x + 282, y - 10, label, size=22)
    d.line(825, 81, 825, 145, arrow=True)
    d.label(813, 120, "独立な和", anchor="end", size=22)
    d.label(124, 232, "¹ n が偶数の場合", size=22)

    d.path([(939, 176), (950, 176), (950, 423)], width=2)
    d.line(950, 311, 941, 311, arrow=True)
    d.line(950, 423, 941, 423, arrow=True)
    for x, y, label, detail in (
        (right, 273, "独立な Z と Q", "Q ∼ χ²_{ν}"),
        (right, 385, "独立な Q₁ と Q₂", "Q₁ ∼ χ²_{m}, Q₂ ∼ χ²_{n}"),
        (middle, 273, "t 分布", "T = Z/√(Q/ν)"),
        (middle, 385, "F(m,n)", "(Q₁/m)/(Q₂/n)"),
        (left, 273, "F(1,ν)", "T²"),
    ):
        d.box(x, y, width, 75, label, detail, fill=RED_FILL)
    for x, y, label in ((middle, 311, "比"), (middle, 423, "比"), (left, 311, "二乗")):
        d.line(x + 341, y, x + width + 4, y, arrow=True)
        d.label(x + 282, y - 10, label, size=22)
    d.write(OUTPUT, "distribution-relationships", 481, "カイ二乗分布を介した分布の関係")


def orthogonal_decomposition():
    d = Diagram()
    axes(d, 260, 44, 664, 346)
    d.label(248, 32, "(X₂ − μ)/σ", math=True)
    d.label(672, 354, "(X₁ − μ)/σ", math=True, anchor="start")
    origin, projection, sample = (260, 346), (430, 176), (505, 251)
    d.line(222, 384, 543, 63, stroke=GUIDE, dash="8 5", arrow=True)
    d.line(118, 204, 282, 368, stroke=GUIDE, dash="8 5")
    d.line(*origin, 151, 237, stroke=INK, dash="6 5", arrow=True)
    d.line(*origin, 498, 252, stroke=INK, arrow=True)
    d.line(*origin, 427, 179, stroke=BLUE, arrow=True)
    d.line(*projection, 499, 245, stroke=RED, dash="10 5", arrow=True)
    d.path([(412, 194), (430, 212), (448, 194)], width=1.7)
    d.dot(*projection)
    d.dot(*sample)
    d.label(166, 177, "偏差方向", anchor="end")
    d.label(447, 141, "平均方向の成分", anchor="end")
    d.label(565, 67, "全成分が等しい方向", anchor="start")
    d.label(495, 185, "偏差の成分", anchor="start")
    d.label(526, 248, "標準化した標本", anchor="start")
    d.write(OUTPUT, "orthogonal-decomposition", 411, "正規標本の平均方向と偏差方向")


def main():
    chi_square()
    student_t()
    fisher_f()
    distribution_relationships()
    orthogonal_decomposition()


if __name__ == "__main__":
    main()
