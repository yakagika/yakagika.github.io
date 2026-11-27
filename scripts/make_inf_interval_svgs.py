#!/usr/bin/env python3
"""Generate confidence-interval figures and double-resolution PNGs."""

import re
from html import escape
from math import sqrt
from pathlib import Path
from random import Random
from statistics import NormalDist, mean
from subprocess import run

OUTPUT = Path(__file__).resolve().parents[1] / "images/fields/inference/inf-interval"
WIDTH = 960
INK = "#22324a"
GUIDE = "#c3cdd9"
BLUE = "#225f99"
RED = "#ac482b"
MATH_FONT = "Times, 'Times New Roman', serif"
BLUE_FILL = "#dceaf6"
RED_FILL = "#f8e3da"
SANS_FONT = "Hiragino Sans, Noto Sans CJK JP, Arial, sans-serif"

POPULATION_SD = 4
SAMPLE_SIZE = 64
STANDARD_ERROR = POPULATION_SD / sqrt(SAMPLE_SIZE)
COVERAGE = 0.95
STANDARD_NORMAL = NormalDist()
CRITICAL_VALUE = STANDARD_NORMAL.inv_cdf((1 + COVERAGE) / 2)
SIMULATION_MEAN = 30
SIMULATION_SEED = 9052
SIMULATION_TRIALS = 40


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
        parts = re.split(r"(X̄ₙ|[LUNXZμσn])", value) if math else [value]
        content = []
        for part in parts:
            if part == "X̄ₙ":
                content.append(
                    '<tspan font-style="italic" text-decoration="overline">X</tspan>'
                    '<tspan font-style="italic" dy="5" font-size="20">n</tspan>'
                    '<tspan dy="-5">&#8203;</tspan>'
                )
            elif math and re.fullmatch(r"[LUNXZμσn]", part):
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

    def write(self, name, height, title):
        OUTPUT.mkdir(parents=True, exist_ok=True)
        svg = OUTPUT / f"{name}.svg"
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



def confidence_limits():
    diagram = Diagram()
    left, right, axis_y = 234, 726, 87
    diagram.line(44, axis_y, 916, axis_y, arrow=True)
    diagram.rect(left, axis_y - 10, right - left, 20, fill=BLUE_FILL)
    diagram.dot(left, axis_y, radius=5)
    diagram.dot(right, axis_y, radius=5)
    diagram.label((left + right) / 2, 58, "信頼区間")
    for x, symbol, name in (
        (left, "L", "下側信頼限界"),
        (right, "U", "上側信頼限界"),
    ):
        diagram.label(x, 129, symbol, math=True)
        diagram.label(x, 159, name)
    diagram.write("confidence-limits", 178, "下側信頼限界と上側信頼限界")


def repeated_intervals():
    diagram = Diagram()
    plot = Plot(diagram, (95, 91, 872, 613), (27.5, 32.5), 1)
    rng = Random(SIMULATION_SEED)
    half_width = CRITICAL_VALUE * STANDARD_ERROR
    mu_x = plot.x(SIMULATION_MEAN)
    diagram.label(mu_x, 78, "母平均")
    diagram.line(mu_x, 90, mu_x, plot.bottom, stroke=INK)
    for draw in range(1, SIMULATION_TRIALS + 1):
        sample_mean = mean(rng.gauss(SIMULATION_MEAN, POPULATION_SD)
                           for _ in range(SAMPLE_SIZE))
        lower, upper = sample_mean - half_width, sample_mean + half_width
        covers_mean = lower <= SIMULATION_MEAN <= upper
        color, dash = (BLUE, None) if covers_mean else (RED, "8 5")
        y = plot.top + 12 * draw
        left, right = plot.x(lower), plot.x(upper)
        diagram.line(left, y, right, y, stroke=color, dash=dash)
        diagram.line(left, y - 4, left, y + 4, stroke=color)
        diagram.line(right, y - 4, right, y + 4, stroke=color)

    diagram.label(plot.left, 40, "抽出回", anchor="start")
    for draw in (1, 10, 20, 30, SIMULATION_TRIALS):
        diagram.label(plot.left, plot.top + 12 * draw + 7, str(draw), anchor="start")
    for x, label, color, dash in (
        (482, "母平均を含む", BLUE, None),
        (692, "母平均を含まない", RED, "8 5"),
    ):
        diagram.line(x, 32, x + 42, 32, stroke=color, dash=dash)
        diagram.label(x + 55, 40, label, anchor="start")

    diagram.line(plot.left, plot.bottom, plot.right + 18, plot.bottom, arrow=True)
    for value in (28, 29, 30, 31, 32):
        label = "μ = 30" if value == SIMULATION_MEAN else str(value)
        x_tick(diagram, plot.x(value), plot.bottom, label)
    diagram.label(plot.right + 18, 674, "収穫量 (個)", anchor="end")
    diagram.write("repeated-confidence-intervals", 692, "反復抽出で求めた母平均の信頼区間")


def sampling_densities():
    diagram = Diagram()
    spread = 4 * POPULATION_SD
    plot = Plot(diagram, (88, 52, 887, 307), (-spread, spread), 0.85)
    observation = NormalDist(0, POPULATION_SD)
    sample_mean = NormalDist(0, STANDARD_ERROR)
    center = plot.x(0)
    diagram.line(center, plot.y(sample_mean.pdf(0)), center, plot.bottom,
                 stroke=INK, dash="5 5")
    plot.curve(observation.pdf, stroke=BLUE)
    plot.curve(sample_mean.pdf, stroke=RED, dash="9 5")
    axes(diagram, plot.left, 45, plot.right + 15, plot.bottom)
    diagram.label(plot.left, 30, "確率密度", anchor="start")
    for value in (0, 0.2, 0.4, 0.6, 0.8):
        y_tick(diagram, plot.left, plot.y(value), f"{value:g}")
    for scale, label in ((-1, "μ − σ"), (0, "μ"), (1, "μ + σ")):
        x_tick(diagram, plot.x(scale * POPULATION_SD), plot.bottom, label)
    diagram.label(plot.right + 15, 340, "収穫量 (個)", anchor="end")
    diagram.line(152, 158, 202, 158, stroke=BLUE)
    diagram.label(215, 166, "個々の観測値", anchor="start")
    diagram.label(215, 199, "σ = 4", math=True, anchor="start")
    diagram.line(545, 118, 595, 118, stroke=RED, dash="9 5")
    diagram.label(608, 126, "標本平均", anchor="start")
    diagram.label(608, 161, "σ/√n = 0.5", math=True, anchor="start")
    diagram.label(608, 195, "n = 64", math=True, anchor="start")
    diagram.write("observation-and-sample-mean-densities", 365, "観測値と標本平均の確率密度")


def normal_coverage():
    diagram = Diagram()
    plot = Plot(diagram, (87, 52, 887, 243), (-4.2, 4.2), 0.43)
    density = STANDARD_NORMAL.pdf
    cutoff = CRITICAL_VALUE
    left, right = plot.x(-cutoff), plot.x(cutoff)
    center = plot.x(0)
    for low, high, fill in (
        (plot.low, -cutoff, RED_FILL),
        (-cutoff, cutoff, BLUE_FILL),
        (cutoff, plot.high, RED_FILL),
    ):
        plot.area(density, low, high, fill)
    plot.curve(density)
    axes(diagram, plot.left, 45, plot.right + 15, plot.bottom)
    diagram.label(plot.left, 30, "確率密度", anchor="start")
    diagram.label(center, 162, "中央 0.95")
    diagram.label(center, 34, "N(0,1)", math=True)
    for side, name in ((-1, "左裾"), (1, "右裾")):
        diagram.label(plot.x(side * 3.0), 176, f"{name} 0.025")
        diagram.line(plot.x(side * 3.0), 186, plot.x(side * 2.40), 233)
    for value, label in ((-cutoff, "−1.96"), (0, "0"), (cutoff, "1.96")):
        x_tick(diagram, plot.x(value), plot.bottom, label)
    diagram.label(plot.right + 27, plot.bottom + 8, "Z", math=True)

    # Solving for the fixed mean reverses the correspondence of the two endpoints.
    original_y = 376
    for start, end in ((left, right), (right, left)):
        diagram.line(start, 283, end, original_y - 12, stroke=GUIDE, dash="6 5")
    diagram.line(plot.left, original_y, plot.right + 15, original_y, arrow=True)
    diagram.rect(left, original_y - 7, right - left, 14, fill=BLUE_FILL)
    for x, label in (
        (left, "X̄ₙ − 1.96σ/√n"),
        (center, "X̄ₙ"),
        (right, "X̄ₙ + 1.96σ/√n"),
    ):
        x_tick(diagram, x, original_y, label)
    diagram.label(plot.right + 27, original_y + 8, "μ", math=True)
    diagram.label(plot.right + 15, 415, "元の単位", anchor="end")
    diagram.write("standard-normal-and-confidence-limits", 438, "標準正規分布の中央と信頼限界")


def main():
    confidence_limits()
    repeated_intervals()
    sampling_densities()
    normal_coverage()


if __name__ == "__main__":
    main()
