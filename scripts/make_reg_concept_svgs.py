#!/usr/bin/env python3
"""Generate the regression teaching diagrams and their double-resolution PNGs."""

import re
from dataclasses import dataclass
from html import escape
from math import exp, sqrt
from pathlib import Path
from statistics import mean
from subprocess import run

ROOT = Path(__file__).resolve().parents[1] / "images" / "fields" / "regression"
WIDTH = 960
INK = "#22324a"
GRAY = "#66758b"
GUIDE = "#c3cdd9"
BLUE = "#225f99"
RED = "#ac482b"
MATH_FONT = "Times, 'Times New Roman', serif"
MATH_VARIABLE = re.compile(r"([a-zA-Zεŷȳ])(ᵢ?)")
POINTS = tuple(enumerate((1.1, 0.7, 1.5, 1.2, 2.6, 2.1, 2.4, 3.6), start=1))


@dataclass(frozen=True)
class Fit:
    intercept: float
    slope: float
    x_mean: float
    y_mean: float
    tss: float
    rss: float

    def at(self, x):
        return self.intercept + self.slope * x

    @property
    def r_squared(self):
        return 1 - self.rss / self.tss


def fit(points):
    xs, ys = zip(*points)
    x_mean, y_mean = mean(xs), mean(ys)
    slope = sum((x - x_mean) * (y - y_mean) for x, y in points) / sum(
        (x - x_mean) ** 2 for x in xs
    )
    intercept = y_mean - slope * x_mean
    tss = sum((y - y_mean) ** 2 for y in ys)
    rss = sum((y - intercept - slope * x) ** 2 for x, y in points)
    return Fit(intercept, slope, x_mean, y_mean, tss, rss)


class Diagram:
    def __init__(self):
        self.elements = []

    def line(self, x1, y1, x2, y2, color=INK, width=3, dashed=False, arrow=False):
        dash = ' stroke-dasharray="10 8"' if dashed else ""
        marker = f' marker-end="url(#arrow-{color[1:]})"' if arrow else ""
        self.elements.append(
            f'<line x1="{x1:.2f}" y1="{y1:.2f}" x2="{x2:.2f}" y2="{y2:.2f}" '
            f'stroke="{color}" stroke-width="{width}"{dash}{marker}/>'
        )

    def point(self, x, y, color=INK, fill=None, radius=7):
        fill = color if fill is None else fill
        self.elements.append(
            f'<circle cx="{x:.2f}" cy="{y:.2f}" r="{radius}" fill="{fill}" '
            f'stroke="{color}" stroke-width="2"/>'
        )

    def label(self, x, y, value, *, color=INK, size=29, anchor="start", math=False):
        content = escape(value)
        if math:
            def variable(match):
                letter, subscript = match.groups()
                suffix = (
                    '<tspan font-size="70%" baseline-shift="-0.23em" dx="-0.06em">i</tspan>'
                    if subscript else ""
                )
                return (
                    f'<tspan font-family="{MATH_FONT}" font-style="italic">'
                    f"{letter}{suffix}</tspan>"
                )

            content = MATH_VARIABLE.sub(variable, content)
        self.elements.append(
            f'<text x="{x:.2f}" y="{y:.2f}" fill="{color}" font-size="{size}" '
            f'text-anchor="{anchor}">{content}</text>'
        )

    def curve(self, points, color=BLUE):
        coords = " ".join(f"{x:.2f},{y:.2f}" for x, y in points)
        self.elements.append(
            f'<polyline points="{coords}" fill="none" stroke="{color}" stroke-width="3"/>'
        )

    def write(self, block, name, height, title):
        svg = ROOT / block / f"{name}.svg"
        png = svg.with_suffix(".png")
        body = "\n  ".join(self.elements)
        markers = "".join(
            f'<marker id="arrow-{color[1:]}" markerWidth="6" markerHeight="6" '
            f'refX="5" refY="3" orient="auto">'
            f'<path d="M0 0 L6 3 L0 6 Z" fill="{color}"/></marker>'
            for color in (INK, BLUE, RED)
        )
        svg.write_text(
            f'<svg xmlns="http://www.w3.org/2000/svg" width="{WIDTH}" height="{height}" '
            f'viewBox="0 0 {WIDTH} {height}" role="img" aria-labelledby="title">\n'
            f'  <title id="title">{escape(title)}</title>\n'
            f"  <defs>{markers}</defs>\n"
            f'  <rect width="{WIDTH}" height="{height}" fill="#fff"/>\n'
            '  <g font-family="Hiragino Sans, Noto Sans CJK JP, Arial, sans-serif">\n'
            f"  {body}\n"
            "  </g>\n"
            "</svg>\n",
            encoding="utf-8",
        )
        run(
            ["rsvg-convert", "-w", str(2 * WIDTH), "-o", str(png), str(svg)],
            check=True,
        )


@dataclass(frozen=True)
class Plot:
    left: int
    right: int
    top: int
    bottom: int
    x_max: float = 9
    y_max: float = 4.5

    def x(self, value):
        return self.left + value / self.x_max * (self.right - self.left)

    def y(self, value):
        return self.bottom - value / self.y_max * (self.bottom - self.top)

    def axes(self, diagram):
        diagram.line(self.left, self.bottom, self.right, self.bottom, GRAY, 2)
        diagram.line(self.left, self.bottom, self.left, self.top, GRAY, 2)
        diagram.label(self.right, self.bottom + 32, "x", anchor="end", math=True)
        diagram.label(self.left - 12, self.top + 4, "y", anchor="end", math=True)

    def line(self, diagram, at):
        diagram.line(self.left, self.y(at(0)), self.right, self.y(at(self.x_max)), INK, 4)

    def points(self, diagram, observations, radius=7):
        for x, y in observations:
            diagram.point(self.x(x), self.y(y), radius=radius)


def population_errors():
    diagram = Diagram()
    left = Plot(47, 292, 86, 324)
    right = Plot(661, 915, 86, 324)
    for plot in (left, right):
        plot.axes(diagram)
        plot.line(diagram, population_mean)
    diagram.label(168, 48, "1 観測の誤差", anchor="middle")
    diagram.label(480, 48, "誤差の正規分布", anchor="middle")
    diagram.label(795, 48, "等分散の仮定", anchor="middle")

    x, y = 4.5, 3.55
    px, py = left.x(x), left.y(y)
    center = left.y(population_mean(x))
    diagram.line(px, center, px, py + 9, RED, 4, arrow=True)
    diagram.point(px, center, INK, "white", 6)
    diagram.point(px, py)
    diagram.label(px, py - 15, "(xᵢ, yᵢ)", anchor="middle", math=True)
    diagram.label(px + 10, (center + py) / 2 + 8, "εᵢ", color=RED, math=True)

    values = [i / 16 for i in range(-48, 49)]
    diagram.line(347, 324, 610, 324, GRAY, 2)
    diagram.line(480, 324, 480, 140, GUIDE, 2, dashed=True)
    diagram.curve((480 + 42 * t, 324 - 182 * exp(-t * t / 2)) for t in values)
    diagram.label(480, 356, "0", anchor="middle")
    diagram.label(610, 356, "ε", anchor="end", math=True)

    for x in (1.6, 4.15, 6.7):
        px, center = right.x(x), right.y(population_mean(x))
        diagram.line(px, center - 51, px, center + 51, GUIDE, 2)
        diagram.curve((px + 44 * exp(-t * t / 2), center + 17 * t) for t in values)
        diagram.point(px, center, INK, "white", 5)
    diagram.write("reg-ols", "population-errors", 386, "母回帰直線と誤差の分布")


def population_mean(x):
    return 1 + 0.29 * x


def candidate(x):
    return 0.70 + 0.26 * x


def least_squares_deviations():
    diagram = Diagram()
    plot = Plot(85, 865, 56, 370)
    plot.axes(diagram)
    plot.line(diagram, candidate)
    diagram.label(
        plot.right + 61, plot.y(candidate(plot.x_max)) - 22, "a + bx",
        anchor="end", math=True,
    )
    for index in (3, 4):
        x, y = POINTS[index]
        px, py = plot.x(x), plot.y(y)
        fitted = plot.y(candidate(x))
        endpoint = py + (10 if py < fitted else -10)
        diagram.line(px, fitted, px, endpoint, RED, 4, arrow=True)
        diagram.point(px, fitted, INK, "white", 5)
    diagram.label(441, 76, "yᵢ − (a + bxᵢ)", color=RED, size=32, math=True)
    plot.points(diagram, POINTS)
    diagram.write(
        "reg-ols", "least-squares-deviations", 426, "候補の直線と観測値の縦の差"
    )


def decomposition(model):
    diagram = Diagram()
    plot = Plot(90, 565, 45, 370, x_max=9.3)
    plot.axes(diagram)
    plot.line(diagram, model.at)
    average = plot.y(model.y_mean)
    diagram.line(plot.left, average, plot.right, average, GRAY, 3, dashed=True)

    x, y = POINTS[-1]
    px, observed, fitted = plot.x(x), plot.y(y), plot.y(model.at(x))
    for height, label in ((observed, "yᵢ"), (fitted, "ŷᵢ"), (average, "ȳ")):
        diagram.line(plot.left, height, 817, height, GUIDE, 2, dashed=True)
        diagram.label(73, height + 10, label, anchor="end", math=True)
    diagram.line(px, plot.bottom, px, observed, GUIDE, 2, dashed=True)
    diagram.label(px, 402, "xᵢ", anchor="middle", math=True)
    plot.points(diagram, POINTS)
    diagram.point(px, fitted, INK, "white", 6)
    for start, end, x, label, color in (
        (average, observed, 623, "yᵢ − ȳ", INK),
        (average, fitted, 764, "ŷᵢ − ȳ", BLUE),
        (fitted, observed, 764, "eᵢ", RED),
    ):
        diagram.line(x, start, x, end + 4, color, 4, arrow=True)
        diagram.label(x + 12, (start + end) / 2 + 10, label, color=color, math=True)
    diagram.write(
        "reg-simple", "decomposition-one-observation", 433, "一つの観測での三つの差"
    )

    diagram = Diagram()
    plot = Plot(76, 886, 96, 396)
    plot.axes(diagram)
    plot.line(diagram, model.at)
    average = plot.y(model.y_mean)
    diagram.line(plot.left, average, plot.right, average, GRAY, 3, dashed=True)
    diagram.label(892, average + 10, "ȳ", color=GRAY, math=True)
    diagram.label(165, 57, "ŷᵢ − ȳ", color=BLUE, size=34, math=True)
    diagram.label(348, 57, "eᵢ = yᵢ − ŷᵢ", color=RED, size=34, math=True)
    for x, y in POINTS:
        px, fitted = plot.x(x), plot.y(model.at(x))
        diagram.line(px - 5, average, px - 5, fitted, BLUE, 5)
        diagram.line(px + 5, fitted, px + 5, plot.y(y), RED, 5)
        diagram.line(px - 5, fitted, px + 5, fitted, INK, 2)
    plot.points(diagram, POINTS)
    diagram.write(
        "reg-simple", "decomposition-all-observations", 452, "複数の観測での二つの差"
    )


def comparison_samples():
    xs = tuple(1 + 7 * i / 23 for i in range(24))
    x_mean = mean(xs)
    # The symmetric deviations have mean zero and are orthogonal to the centered x.
    half = (1, -2, 1, 2, -1, 0, -2, 2, -1, 1, -2, 1)
    deviations = half + half[::-1]
    scale = sqrt(sum((x - x_mean) ** 2 for x in xs) / sum(d ** 2 for d in deviations))
    return [
        tuple(
            (x, 2.25 + 0.38 * (sqrt(r2) * (x - x_mean) + sqrt(1 - r2) * scale * d))
            for x, d in zip(xs, deviations)
        )
        for r2 in (0.90, 0.20)
    ]


def fit_comparison(samples, models):
    for residuals, name in (
        (False, "r-squared-fits"),
        (True, "r-squared-residuals"),
    ):
        diagram = Diagram()
        for offset, observations, model in zip((0, 476), samples, models):
            plot = Plot(52 + offset, 449 + offset, 105, 368)
            plot.axes(diagram)
            plot.line(diagram, model.at)
            if residuals:
                for x, y in observations:
                    px = plot.x(x)
                    diagram.line(px, plot.y(model.at(x)), px, plot.y(y), RED, 4)
                label = f"残差平方和 {model.rss:.2f}"
            else:
                label = f"R² = {model.r_squared:.2f}"
            diagram.label(
                (plot.left + plot.right) / 2, 62, label, size=31, anchor="middle",
                math=not residuals,
            )
            plot.points(diagram, observations, radius=5)
        title = "当てはまりの異なる二つの標本"
        if residuals:
            title = "当てはまりの異なる二つの標本の残差"
        diagram.write("reg-simple", name, 423, title)


def main():
    model = fit(POINTS)
    samples = comparison_samples()
    models = [fit(points) for points in samples]
    population_errors()
    least_squares_deviations()
    decomposition(model)
    fit_comparison(samples, models)


if __name__ == "__main__":
    main()
