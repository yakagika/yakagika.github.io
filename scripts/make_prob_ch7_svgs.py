#!/usr/bin/env python3
"""Generate chapter 7 probability figures and their double-resolution PNGs."""

from html import escape
from itertools import product
from math import comb, exp, factorial, log, log10, sqrt, tau
from pathlib import Path
from random import Random
import re
from subprocess import run

ROOT = Path(__file__).resolve().parents[1] / "images/fields/probability"
VARIABLE = "prob-variable"
DISCRETE = "prob-discrete"
WIDTH = 960
INK = "#22324a"
GUIDE = "#c3cdd9"
BLUE = "#225f99"
RED = "#ac482b"
MATH_FONT = "Times, 'Times New Roman', serif"
BLUE_FILL = "#dceaf6"
RED_FILL = "#f8e3da"
SANS_FONT = "Hiragino Sans, Noto Sans CJK JP, Arial, sans-serif"

POPULATION = "母集団分布"
OBSERVATIONS = "手元のデータ"
ESTIMATE = "データから推定した分布"
WEIGHT = "ウサギの体重 (kg)"
# Illustrative counts in equal bins from 1 to 3 kg.
RABBIT_COUNTS = (1, 4, 9, 11, 10, 7, 3, 1)
COIN_GROUPS = tuple(
    tuple(outcome for outcome in product((1, 0), repeat=3) if sum(outcome) == heads)
    for heads in range(4)
)
COIN_SEED = 7
COIN_TRIALS = 10_000


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

    def label(self, x, y, value, *, size=24, math=False, anchor="middle"):
        content = escape(value)
        font = ""
        if math:
            font = f' font-family="{MATH_FONT}"'
            content = re.sub(r"([XxNpabSuσωλ])", r'<tspan font-style="italic">\1</tspan>',
                             content)
            content = content.replace(
                "²", '<tspan font-size="84%" baseline-shift="super">2</tspan>',
            )
        self.elements.append(
            f'<text x="{x:.2f}" y="{y:.2f}" fill="{INK}" font-size="{size}" '
            f'text-anchor="{anchor}"{font}>{content}</text>'
        )

    def path(self, points, *, stroke=INK, width=3, fill="none"):
        coords = " L ".join(f"{x:.2f} {y:.2f}" for x, y in points)
        close = " Z" if fill != "none" else ""
        self.elements.append(
            f'<path d="M {coords}{close}" fill="{fill}" stroke="{stroke}" '
            f'stroke-width="{width}" stroke-linejoin="round"/>'
        )

    def dot(self, x, y):
        self.elements.append(f'<circle cx="{x}" cy="{y}" r="4" fill="{INK}"/>')

    def write(self, block, name, height, title):
        folder = ROOT / block
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


def x_tick(diagram, x, bottom, label):
    diagram.line(x, bottom, x, bottom + 7)
    diagram.label(x, bottom + 32, label, math=True)


def y_tick(diagram, left, y, label):
    diagram.line(left - 7, y, left, y)
    diagram.label(left - 13, y + 8, label, math=True, anchor="end")


def normal_density(value, mean, deviation):
    return exp(-0.5 * ((value - mean) / deviation) ** 2) / (deviation * sqrt(tau))


def rabbit_plot(diagram, left, right, title):
    top, bottom = 100, 247
    plot_right = right - 12
    span = plot_right - left
    diagram.label((left + plot_right) / 2, 34, title, size=26)
    if title == OBSERVATIONS:
        diagram.label(left, 86, "個体数", anchor="start")
        bin_width = span / len(RABBIT_COUNTS)
        for index, count in enumerate(RABBIT_COUNTS):
            height = (bottom - top) * count / 13
            diagram.rect(left + index * bin_width, bottom - height, bin_width, height,
                         BLUE_FILL)
        for count in (4, 8):
            y_tick(diagram, left, bottom - (bottom - top) * count / 13, str(count))
    else:
        diagram.label(left, 86, "確率密度", anchor="start")
        if title == POPULATION:
            # A lognormal population gives one peak and a right tail.
            density = lambda x: normal_density(log(x), log(1.9), 0.20) / x
        else:
            midpoints = [1 + (i + 0.5) / 4 for i in range(len(RABBIT_COUNTS))]
            sample_size = sum(RABBIT_COUNTS)
            mean = sum(x * n for x, n in zip(midpoints, RABBIT_COUNTS)) / sample_size
            deviation = sqrt(
                sum(n * (x - mean) ** 2 for x, n in zip(midpoints, RABBIT_COUNTS))
                / (sample_size - 1)
            )
            density = lambda x: normal_density(x, mean, deviation)
        diagram.path([
            (left + span * i / 160, bottom - (bottom - top) * density(1 + i / 80) / 1.2)
            for i in range(161)
        ], stroke=BLUE)
        for density_tick in (0.4, 0.8):
            y_tick(diagram, left, bottom - (bottom - top) * density_tick / 1.2,
                   str(density_tick))
    axes(diagram, left, top, right, bottom)
    for weight in (1, 2, 3):
        x_tick(diagram, left + span * (weight - 1) / 2, bottom, str(weight))
    diagram.label((left + right) / 2, 314, WEIGHT)


def rabbit_observations():
    diagram = Diagram()
    rabbit_plot(diagram, 65, 449, POPULATION)
    rabbit_plot(diagram, 540, 933, OBSERVATIONS)
    diagram.line(480, 20, 480, 312, stroke=GUIDE)
    diagram.write(VARIABLE, "rabbit-population-and-sample", 334,
                  "ウサギの体重の母集団分布と手元のデータ")


def rabbit_inference():
    diagram = Diagram()
    rabbit_plot(diagram, 65, 399, OBSERVATIONS)
    rabbit_plot(diagram, 561, 933, ESTIMATE)
    diagram.line(418, 169, 520, 169, arrow=True)
    diagram.label(469, 127, "分布の推定")
    diagram.label(232, 358, "記述統計")
    diagram.label(747, 358, "推測統計")
    diagram.write(VARIABLE, "rabbit-description-and-inference", 377,
                  "手元のデータの要約とデータから推定した分布")


def rabbit_variances():
    diagram = Diagram()
    panels = (
        (46, 295, POPULATION, "母分散", "σ²"),
        (366, 615, OBSERVATIONS, "データの分散", "S²"),
        (686, 935, ESTIMATE, "母分散の推定値", "u²"),
    )
    for left, right, title, variance, symbol in panels:
        rabbit_plot(diagram, left, right, title)
        center = (left + right) / 2
        diagram.line(center, 329, center, 352, arrow=True)
        diagram.label(center, 385, variance)
        diagram.label(center, 419, symbol, math=True, size=28)
    for x in (320, 640):
        diagram.line(x, 18, x, 426, stroke=GUIDE)
    diagram.write(VARIABLE, "rabbit-three-variances", 436,
                  "母集団分布, データ, 推定した分布に対応する分散")


def variance_calculation():
    diagram = Diagram()
    rows = (
        (66, "既知の分布", "母集団分布", "分散の計算", "母分散", "σ²"),
        (211, "未知の分布", "手元のデータ", "母分散の推定", "推定値", "u²"),
    )
    for y, condition, source, operation, target, symbol in rows:
        diagram.label(30, y - 14, condition, anchor="start")
        diagram.rect(30, y, 305, 68, BLUE_FILL)
        diagram.label(182, y + 43, source, size=28)
        diagram.line(356, y + 34, 631, y + 34, arrow=True)
        diagram.label(494, y + 23, operation)
        diagram.rect(654, y, 276, 68, RED_FILL)
        diagram.label(745, y + 43, target, size=28)
        diagram.label(858, y + 43, symbol, math=True, size=28)
    diagram.write(VARIABLE, "variance-calculation-and-estimation", 298,
                  "既知の分布からの分散の計算と未知の母分散の推定")


def triple(outcome):
    return "(" + ", ".join(map(str, outcome)) + ")"


def coin_event():
    diagram = Diagram()
    diagram.rect(36, 18, 888, 229)
    diagram.label(56, 51, "Ω", math=True, anchor="start", size=28)
    diagram.rect(56, 70, 848, 77, BLUE_FILL)
    diagram.label(161, 103, "表が 2 回以上")
    diagram.label(134, 132, "確率")
    diagram.label(184, 132, "1/2", math=True)
    for row, outcomes in enumerate((COIN_GROUPS[3] + COIN_GROUPS[2],
                                    COIN_GROUPS[1] + COIN_GROUPS[0])):
        for index, outcome in enumerate(outcomes):
            diagram.label(340 + 151 * index, 116 + 93 * row, triple(outcome),
                          math=True, size=28)
    diagram.write(VARIABLE, "three-toss-event", 264, "コイン 3 回の標本空間と表が 2 回以上の事象")


def coin_groups():
    diagram = Diagram()
    diagram.label(36, 37, "表の回数", anchor="start")
    diagram.label(147, 37, "x", math=True, anchor="start")
    for heads, outcomes in enumerate(COIN_GROUPS):
        left = 36 + heads * 225
        center = left + 106
        diagram.rect(left, 55, 212, 231, BLUE_FILL)
        diagram.label(center, 90, f"x = {heads}", math=True, size=28)
        diagram.line(left + 14, 105, left + 198, 105, stroke=GUIDE)
        for index, outcome in enumerate(outcomes):
            diagram.label(center, 142 + index * 42, triple(outcome), math=True, size=28)
        diagram.line(left + 14, 240, left + 198, 240, stroke=GUIDE)
        diagram.label(center, 271, f"P(X = {heads}) = {len(outcomes)}/8", math=True)
    diagram.write(VARIABLE, "three-toss-count-groups", 303, "表の回数で分けたコイン 3 回の結果")


def coin_mapping():
    diagram = Diagram()
    diagram.rect(27, 17, 906, 99)
    diagram.label(50, 49, "ω ∈ Ω", math=True, anchor="start", size=28)
    diagram.label(480, 167, "X: Ω → ℝ", math=True, size=28)
    bottom = 239
    diagram.line(39, bottom, 921, bottom, arrow=True)
    diagram.label(923, 217, "ℝ", math=True, size=28)

    # Keep the source order aligned with the target ticks so arrows do not cross.
    source_x = 76
    source_y = 117
    tip_y = bottom - 25
    fraction = (tip_y - source_y) / (bottom - source_y)
    for heads, outcomes in enumerate(COIN_GROUPS):
        target_x = 119 + heads * 240
        for outcome in outcomes:
            diagram.label(source_x, 91, triple(outcome), math=True, size=26)
            # Each arrow points at the dot, with space between converging tips.
            tip_x = source_x + fraction * (target_x - source_x)
            diagram.line(source_x, source_y, tip_x, tip_y, arrow=True)
            source_x += 115
        x_tick(diagram, target_x, bottom, str(heads))
        diagram.dot(target_x, bottom)
    diagram.label(919, 271, "x", math=True)
    diagram.write(VARIABLE, "three-toss-random-variable", 289,
                  "表の回数を表す標本空間から実数直線への写像")


def mass_plot(diagram, left, right, top, bottom, bars, ymax, y_ticks):
    step = (right - left - 24) / len(bars)
    bar_width = min(58, step * 0.66)
    diagram.label(left, top - 16, "p(x) = P(X = x)", math=True, anchor="start")
    for value, label in y_ticks:
        y = bottom - (bottom - top) * value / ymax
        diagram.line(left, y, right - 14, y, stroke=GUIDE)
        y_tick(diagram, left, y, label)
    for index, (label, mass) in enumerate(bars):
        x = left + step * (index + 0.5)
        if mass is None:
            diagram.label(x, bottom + 32, label, math=True)
            continue
        height = (bottom - top) * mass / ymax
        diagram.rect(x - bar_width / 2, bottom - height, bar_width, height, BLUE_FILL)
        x_tick(diagram, x, bottom, label)
    axes(diagram, left, top, right, bottom)
    y_tick(diagram, left, bottom, "0")
    diagram.label(right, bottom + 32, "x", math=True, anchor="end")


def die_mass():
    diagram = Diagram()
    bars = [(str(face), 1 / 6) for face in range(1, 7)]
    mass_plot(diagram, 105, 910, 58, 233, bars, 0.23, [(1 / 6, "1/6")])
    diagram.label(504, 299, "出た目")
    diagram.write(VARIABLE, "fair-die-mass", 317, "公正なサイコロの各目の確率")


def discrete_and_continuous():
    diagram = Diagram()
    diagram.label(252, 31, "離散型確率分布", size=26)
    diagram.label(721, 31, "連続型確率分布", size=26)
    diagram.line(480, 17, 480, 314, stroke=GUIDE)
    mass_plot(diagram, 52, 446, 111, 266, [("0", 0.25), ("1", 0.5), ("2", 0.25)],
              0.7, [(0.5, "1/2")])

    left, right, bottom = 539, 921, 266
    diagram.label(left, 95, "確率密度", anchor="start")
    a, b = 678, 862

    def curve(x):
        return bottom - 139 * exp(-0.5 * ((x - 736) / 70) ** 2)

    interval = [(x, curve(x)) for x in range(a, b + 1, 2)]
    diagram.path([(a, bottom), *interval, (b, bottom)], fill=RED_FILL, width=2)
    diagram.path([(x, curve(x)) for x in range(left, right - 4, 2)])
    axes(diagram, left, 111, right, bottom)
    x_tick(diagram, a, bottom, "a")
    x_tick(diagram, b, bottom, "b")
    diagram.label(757, 234, "P(a ≤ X ≤ b)", math=True)
    diagram.label(right, 298, "x", math=True)
    diagram.write(VARIABLE, "discrete-and-continuous", 326, "確率質量の棒の高さと確率密度の下の面積")


def uniform_mass():
    diagram = Diagram()
    bars = [("1", 1), ("2", 1), ("⋯", None), ("N", 1)]
    # The symbolic mass 1/N sets the vertical scale.
    mass_plot(diagram, 105, 910, 58, 233, bars, 1.35, [(1, "1/N")])
    diagram.write(DISCRETE, "discrete-uniform-mass", 282, "離散一様分布の確率質量")


def coin_relative_frequency():
    diagram = Diagram()
    left, right, top, bottom = 103, 907, 73, 326
    span = right - left - 10
    height = bottom - top

    def x_at(trials):
        return left + span * log10(trials) / log10(COIN_TRIALS)

    def y_at(frequency):
        return bottom - height * frequency

    diagram.label(left, 44, "表の相対度数", anchor="start")
    for value, label in ((0, "0"), (0.25, "0.25"), (0.5, "0.5"),
                         (0.75, "0.75"), (1, "1")):
        y_tick(diagram, left, y_at(value), label)
    for trials in (1, 10, 100, 1000, COIN_TRIALS):
        x_tick(diagram, x_at(trials), bottom, str(trials))
    diagram.line(left, y_at(0.5), right, y_at(0.5), stroke=RED, dash="8 6")
    diagram.label(889, y_at(0.5) - 14, "p = 1/2", math=True, anchor="end")

    random = Random(COIN_SEED)
    heads = 0
    points = []
    for trials in range(1, COIN_TRIALS + 1):
        heads += random.randrange(2)
        points.append((x_at(trials), y_at(heads / trials)))
    diagram.path(points, stroke=BLUE, width=2.5)
    axes(diagram, left, top - 9, right + 12, bottom)
    diagram.label((left + right) / 2, 392, "試行回数 (対数目盛)")
    diagram.write(DISCRETE, "coin-relative-frequency", 411, "コイン投げの表の相対度数の推移")


def binomial_mass():
    diagram = Diagram()
    n = 10
    bars = [(str(k), comb(n, k) / 2**n) for k in range(n + 1)]
    diagram.label(480, 36, "Bi(10, 1/2)", math=True, size=28)
    ticks = [(value, str(value)) for value in (0.05, 0.10, 0.15, 0.20, 0.25)]
    mass_plot(diagram, 105, 910, 89, 296, bars, 0.28, ticks)
    diagram.write(DISCRETE, "binomial-mass", 344, "10 回のコイン投げの表の回数の確率質量")


def poisson_mass():
    diagram = Diagram()
    rate = 2
    bars = [(str(k), exp(-rate) * rate**k / factorial(k)) for k in range(11)]
    bars.append(("⋯", None))
    diagram.label(480, 36, "Po(2)", math=True, size=28)
    ticks = [(value, str(value)) for value in (0.1, 0.2, 0.3)]
    mass_plot(diagram, 105, 910, 89, 296, bars, 0.33, ticks)
    diagram.write(DISCRETE, "poisson-mass", 344, "平均 2 のポアソン分布の確率質量")


def main():
    rabbit_observations()
    rabbit_inference()
    rabbit_variances()
    variance_calculation()
    coin_event()
    coin_groups()
    coin_mapping()
    die_mass()
    discrete_and_continuous()
    uniform_mass()
    coin_relative_frequency()
    binomial_mass()
    poisson_mass()


if __name__ == "__main__":
    main()
