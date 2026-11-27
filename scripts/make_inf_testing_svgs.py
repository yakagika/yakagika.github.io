#!/usr/bin/env python3
"""Generate hypothesis-testing figures and double-resolution PNGs."""

from html import escape
from math import comb
from pathlib import Path
from statistics import NormalDist
from subprocess import run

OUTPUT = Path(__file__).resolve().parents[1] / "images/fields/inference"
TESTING = "inf-testing"
MEAN_TESTS = "inf-mean-variance-tests"
WIDTH = 960
INK = "#22324a"
GUIDE = "#c3cdd9"
BLUE = "#225f99"
RED = "#ac482b"
MATH_FONT = "Times, 'Times New Roman', serif"
BLUE_FILL = "#dceaf6"
RED_FILL = "#f8e3da"
SANS_FONT = "Hiragino Sans, Noto Sans CJK JP, Arial, sans-serif"

TRIALS = 10
EXAMPLE_CORRECT = 7
BINOMIAL = tuple(comb(TRIALS, k) / 2**TRIALS for k in range(TRIALS + 1))
NORMAL = NormalDist()
ALPHA = 0.05
TWO_SIDED = NORMAL.inv_cdf(1 - ALPHA / 2)
ONE_SIDED = NORMAL.inv_cdf(1 - ALPHA)


class Diagram:
    def __init__(self):
        self.elements = []

    def rect(self, x, y, width, height, fill):
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
        content = escape(value)
        if value == "X̄ₙ":
            content = (
                '<tspan font-style="italic" text-decoration="overline">X</tspan>'
                '<tspan font-style="italic" dy="5" font-size="20">n</tspan>'
            )
        self.elements.append(
            f'<text x="{x:.2f}" y="{y:.2f}" fill="{fill}" font-size="{size}" '
            f'text-anchor="{anchor}"{font}>{content}</text>'
        )

    def path(self, points, *, width=2, fill="none"):
        coords = " L ".join(f"{x:.2f} {y:.2f}" for x, y in points)
        close = " Z" if fill != "none" else ""
        self.elements.append(
            f'<path d="M {coords}{close}" fill="{fill}" stroke="{INK}" '
            f'stroke-width="{width}" stroke-linejoin="round"/>'
        )

    def dot(self, x, y, *, radius=5, fill=INK):
        self.elements.append(
            f'<circle cx="{x:.2f}" cy="{y:.2f}" r="{radius}" fill="{fill}" '
            f'stroke="{INK}" stroke-width="2"/>'
        )

    def write(self, block, name, height, title):
        directory = OUTPUT / block
        directory.mkdir(parents=True, exist_ok=True)
        svg = directory / f"{name}.svg"
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


def tick(diagram, x, bottom, value):
    diagram.line(x, bottom, x, bottom + 7)
    diagram.label(x, bottom + 32, value, math=True)


def bracket(diagram, left, right, y, label):
    diagram.path([(left, y - 6), (left, y), (right, y), (right, y - 6)])
    diagram.label((left + right) / 2, y + 28, label, size=20)


def binomial_plot(diagram, left, right, bottom, red_bars=(), blue_bars=()):
    step = (right - left) / 11.5

    def x(k):
        return left + step * (k + 0.7)

    for k, probability in enumerate(BINOMIAL):
        bar_height = 620 * probability
        fill = "white"
        if k in red_bars:
            fill = RED_FILL
        elif k in blue_bars:
            fill = BLUE_FILL
        diagram.rect(x(k) - 0.32 * step, bottom - bar_height,
                     0.64 * step, bar_height, fill)
        tick(diagram, x(k), bottom, str(k))

    for probability in (0.1, 0.2):
        y = bottom - 620 * probability
        diagram.line(left - 6, y, left, y)
        diagram.label(left - 11, y + 7, f"{probability:.1f}", size=20,
                      math=True, anchor="end")
    diagram.line(left, bottom, left, bottom - 174, arrow=True)
    diagram.line(left, bottom, right + 10, bottom, arrow=True)
    diagram.label(left, bottom - 184, "P(T = k)", size=21, math=True)
    diagram.label(right + 10, bottom + 34, "k", math=True)
    return x


def tea_experiment():
    diagram = Diagram()
    diagram.label(218, 34, "1 回の正解確率")
    diagram.label(218, 72, "p", size=28, math=True)
    diagram.line(218, 83, 218, 112, arrow=True)
    diagram.label(218, 143, "10 回の判定例")
    for k in range(TRIALS):
        diagram.label(47 + 38 * k, 184, "○" if k < EXAMPLE_CORRECT else "×",
                      size=28)
    diagram.label(218, 216, "○ 正解　× 不正解", size=20)
    diagram.line(218, 227, 218, 253, arrow=True)
    diagram.label(218, 281, "正解回数 T の観測値")
    diagram.label(218, 315, f"t = {EXAMPLE_CORRECT}", size=26, math=True)
    diagram.line(435, 178, 486, 178, arrow=True)
    diagram.label(704, 34, "H₀: p = 1/2", size=26, math=True)
    diagram.label(704, 67, "Bi(10, 1/2)", size=26, math=True)
    x = binomial_plot(diagram, 545, 916, 270, blue_bars=(EXAMPLE_CORRECT,))
    diagram.line(x(EXAMPLE_CORRECT), 309, x(EXAMPLE_CORRECT), 320, stroke=BLUE)
    diagram.label(x(EXAMPLE_CORRECT), 344, "観測値", size=20, fill=BLUE)
    diagram.write(TESTING, "tea-experiment", 364,
                  "紅茶を見分ける確率と正解回数の分布")


def reasoning_and_testing():
    diagram = Diagram()
    rows = (
        (57, "背理法", ("仮定", "論理的な推論", "論理矛盾", "仮定の否定")),
        (140, "仮説検定", ("帰無仮説 H₀", "棄却域の確率 ≤ α", "棄却域内の t", "H₀ の棄却")),
    )
    for y, name, steps in rows:
        diagram.label(13, y + 8, name, size=22, anchor="start")
        for index, step in enumerate(steps):
            x = 124 + 211 * index
            diagram.rect(x, y - 26, 188, 52, BLUE_FILL if index < 2 else RED_FILL)
            diagram.label(x + 94, y + 8, step, size=21)
            if index < len(steps) - 1:
                diagram.line(x + 191, y, x + 208, y, arrow=True)
    diagram.write(TESTING, "contradiction-and-testing", 190,
                  "背理法と仮説検定の比較")


def binomial_rejection():
    diagram = Diagram()
    diagram.label(480, 32, "Bi(10, 1/2)", size=28, math=True)
    diagram.label(480, 63, "α = 0.05", math=True)
    for left, right, title, ranges in (
        (61, 431, "両側検定", ((0, 1), (9, 10))),
        (541, 911, "右片側検定", ((9, 10),)),
    ):
        diagram.label((left + right) / 2, 104, title)
        rejection = tuple(k for lo, hi in ranges for k in range(lo, hi + 1))
        x = binomial_plot(diagram, left, right, 318, red_bars=rejection)
        for lo, hi in ranges:
            bracket(diagram, x(lo) - 11, x(hi) + 11, 367, "棄却域")
    diagram.write(TESTING, "binomial-rejection-regions", 411,
                  "二項検定の両側と右片側の棄却域")


class NormalPlot:
    def __init__(self, diagram, left, right, bottom, height):
        self.diagram = diagram
        self.left = left
        self.right = right
        self.bottom = bottom
        self.height = height

    def x(self, z):
        return self.left + (self.right - self.left) * (z + 4) / 8

    def y(self, z):
        return self.bottom - self.height * NORMAL.pdf(z) / NORMAL.pdf(0)

    def points(self, low, high):
        values = [low + (high - low) * step / 200 for step in range(201)]
        return [(self.x(z), self.y(z)) for z in values]

    def area(self, low, high, fill):
        self.diagram.path(
            [(self.x(low), self.bottom), *self.points(low, high),
             (self.x(high), self.bottom)],
            width=1.5, fill=fill,
        )

    def axes(self, xlabel):
        self.diagram.path(self.points(-4, 4), width=3)
        self.diagram.line(self.left, self.bottom, self.right + 18, self.bottom,
                          arrow=True)
        self.diagram.label(self.right + 18, self.bottom + 33, xlabel, math=True)

    def tick(self, z, value):
        tick(self.diagram, self.x(z), self.bottom, value)

    def observed(self, z, symbol, label_offset=0):
        x = self.x(z)
        self.diagram.line(x + label_offset, self.bottom - 50, x,
                          self.bottom - 8, arrow=True)
        self.diagram.dot(x, self.bottom)
        self.diagram.label(x + label_offset, self.bottom - 60, symbol,
                           size=26, math=True)


def two_sided_areas(plot):
    plot.area(-4, -TWO_SIDED, RED_FILL)
    plot.area(-TWO_SIDED, TWO_SIDED, BLUE_FILL)
    plot.area(TWO_SIDED, 4, RED_FILL)


def two_sided_labels(plot):
    diagram = plot.diagram
    for sign in (-1, 1):
        x = plot.x(sign * 3)
        diagram.label(x, plot.bottom - 81, "棄却域", size=20)
        diagram.label(x, plot.bottom - 55, "0.025", size=22, math=True)
        diagram.line(x, plot.bottom - 45, plot.x(sign * 2.3), plot.bottom - 8,
                     stroke=INK)
    x = plot.x(0)
    diagram.label(x, plot.bottom - 85, "非棄却域", size=20)
    diagram.label(x, plot.bottom - 57, "0.95", math=True)


def continuous_rejection():
    diagram = Diagram()
    plot = NormalPlot(diagram, 67, 887, 253, 175)
    two_sided_areas(plot)
    plot.axes("T")
    plot.tick(-TWO_SIDED, "−1.960")
    plot.tick(0, "0")
    plot.tick(TWO_SIDED, "1.960")
    two_sided_labels(plot)
    plot.observed(1.2, "t")
    diagram.label(480, 42, "H₀ のもとでの密度")
    diagram.label(480, 317, "N(0, 1)", math=True)
    diagram.write(TESTING, "continuous-rejection-regions", 338,
                  "両側検定で棄却域と観測値を比較する模式図")


def one_sided_p_value():
    diagram = Diagram()
    diagram.label(480, 36, "H₀ のもとでの密度")
    diagram.label(480, 68, "N(0, 1)", math=True)
    for left, observed, title in ((32, 0.95, "p 値 > 0.05"),
                                  (507, 2.3, "p 値 ≤ 0.05")):
        plot = NormalPlot(diagram, left, left + 390, 290, 155)
        plot.area(observed, 4, BLUE_FILL)
        plot.axes("T")
        plot.tick(0, "0")
        plot.tick(ONE_SIDED, "1.645")
        plot.observed(observed, "t", label_offset=-24 if observed < ONE_SIDED else 0)
        diagram.label(plot.x(0), 104, title)
        label_x = plot.x(3.0)
        diagram.label(label_x, 171, "p 値", fill=BLUE)
        diagram.line(label_x, 182, plot.x(observed + 0.23),
                     (plot.y(observed + 0.23) + plot.bottom) / 2)
        bracket(diagram, plot.x(ONE_SIDED), plot.x(4), 344, "棄却域 (0.05)")
    diagram.write(TESTING, "right-sided-p-value", 388,
                  "右片側検定の p 値と棄却域")


def standardizing_mean():
    diagram = Diagram()
    diagram.label(480, 34, "H₀: μ = μ₀", size=26, math=True)
    for left, title, center, ticks, xlabel in (
        (35, "N(μ₀, σ²/n)", "μ₀", ("μ₀ − σ/√n", "μ₀ + σ/√n"), "X̄ₙ"),
        (575, "N(0, 1)", "0", ("−1", "1"), "Z"),
    ):
        plot = NormalPlot(diagram, left, left + 330, 255, 125)
        diagram.label(plot.x(0), 103, title, math=True)
        for z in (-1, 1):
            diagram.line(plot.x(z), plot.y(z), plot.x(z), plot.bottom,
                         stroke=GUIDE, dash="5 4")
        plot.axes(xlabel)
        plot.tick(0, center)
        for z, label, anchor in zip((-1, 1), ticks, ("end", "start")):
            diagram.line(plot.x(z), plot.bottom, plot.x(z), plot.bottom + 7)
            diagram.label(plot.x(z), plot.bottom + 62, label, size=22,
                          math=True, anchor=anchor)
    diagram.label(470, 154, "− μ₀", size=24, math=True)
    diagram.line(393, 168, 546, 168, arrow=True)
    diagram.label(470, 200, "÷ (σ/√n)", size=24, math=True)
    diagram.write(MEAN_TESTS, "standardizing-sample-mean", 335,
                  "帰無仮説のもとでの標本平均の標準化")


def z_test_decisions():
    diagram = Diagram()
    diagram.label(480, 33, "H₀ のもとでの密度")
    diagram.label(480, 64, "N(0, 1)", math=True)
    for left, observed, title in ((34, 0.8, "非棄却"), (514, 2.65, "棄却")):
        plot = NormalPlot(diagram, left, left + 390, 278, 145)
        two_sided_areas(plot)
        plot.axes("Z")
        plot.tick(-TWO_SIDED, "−1.960")
        plot.tick(0, "0")
        plot.tick(TWO_SIDED, "1.960")
        plot.observed(observed, "z")
        diagram.label(plot.x(0), 105, title)
        for q, label in ((-3.0, "0.025"), (0, "0.95"), (3.0, "0.025")):
            diagram.label(plot.x(q), 348, label, math=True)
        for lo, hi, label in ((-4, -TWO_SIDED, "棄却域"),
                              (-TWO_SIDED, TWO_SIDED, "非棄却域"),
                              (TWO_SIDED, 4, "棄却域")):
            bracket(diagram, plot.x(lo) + 4, plot.x(hi) - 4, 373, label)
    diagram.write(MEAN_TESTS, "two-sided-z-test-decisions", 417,
                  "両側 Z 検定で観測値が非棄却域と棄却域に入る場合")


def observation_designs():
    diagram = Diagram()
    diagram.line(480, 22, 480, 320, stroke=GUIDE)
    diagram.label(240, 37, "対応あり")
    diagram.label(720, 37, "対応なし")
    diagram.label(102, 100, "調整前")
    diagram.label(290, 100, "調整後")
    diagram.label(386, 100, "差")
    diagram.label(720, 92, "別々の対象")

    for index in range(3):
        y = 151 + 64 * index
        diagram.line(121, y, 271, y)
        diagram.dot(102, y, radius=18, fill=BLUE_FILL)
        diagram.dot(290, y, radius=18, fill=BLUE_FILL)
        for x in (102, 290):
            diagram.label(x, y + 7, str(index + 1), size=20, math=True)
        diagram.label(386, y + 8, f"D{chr(0x2081 + index)}", math=True)

    for index in range(6):
        x = 570 + 60 * index
        diagram.dot(x, 124, radius=13, fill="white")
    diagram.line(720, 146, 720, 173)
    diagram.label(720, 197, "無作為割付", size=22)
    diagram.line(660, 207, 610, 248, arrow=True)
    diagram.line(780, 207, 830, 248, arrow=True)
    for center, name, fill in ((610, "処理群", RED_FILL), (830, "対照群", BLUE_FILL)):
        for offset in (-40, 0, 40):
            diagram.dot(center + offset, 273, radius=13, fill=fill)
        diagram.label(center, 320, name)
    diagram.write(MEAN_TESTS, "paired-and-independent-observations", 341,
                  "対応のある観測と無作為割付による対応のない観測")


if __name__ == "__main__":
    tea_experiment()
    reasoning_and_testing()
    binomial_rejection()
    continuous_rejection()
    one_sided_p_value()
    standardizing_mean()
    z_test_decisions()
    observation_designs()
