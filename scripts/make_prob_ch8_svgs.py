#!/usr/bin/env python3
"""Generate chapter 8 probability figures and double-resolution PNGs."""

import re
from html import escape
from math import comb, erf, erfc, exp, gamma, sqrt, tau
from pathlib import Path
from random import Random
from subprocess import run

ROOT = Path(__file__).resolve().parents[1] / "images/fields/probability"
CONTINUOUS = "prob-continuous"
NORMAL = "prob-normal"
CLT = "prob-clt"
WIDTH = 960
INK = "#22324a"
GUIDE = "#c3cdd9"
BLUE = "#225f99"
RED = "#ac482b"
MATH_FONT = "Times, 'Times New Roman', serif"
BLUE_FILL = "#dceaf6"
RED_FILL = "#f8e3da"
SANS_FONT = "Hiragino Sans, Noto Sans CJK JP, Arial, sans-serif"

WAIT_SEED = 805
WAIT_TRIALS = 12_000


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
            parts = re.split(r"(Ga|Bi|Ex|[abfknptux-zNXYZSαμφσλ])", value)
            parts = [escape(part) for part in parts]
            for index in range(1, len(parts), 2):
                if len(parts[index]) == 1:
                    parts[index] = f'<tspan font-style="italic">{parts[index]}</tspan>'
            content = "".join(parts)
        self.elements.append(
            f'<text x="{x:.2f}" y="{y:.2f}" fill="{INK}" font-size="{size}" '
            f'text-anchor="{anchor}"{font}>{content}</text>'
        )

    def path(self, points, *, stroke=INK, width=3, fill="none", dash=None):
        coords = " L ".join(f"{x:.2f} {y:.2f}" for x, y in points)
        close = " Z" if fill != "none" else ""
        pattern = f' stroke-dasharray="{dash}"' if dash else ""
        self.elements.append(
            f'<path d="M {coords}{close}" fill="{fill}" stroke="{stroke}" '
            f'stroke-width="{width}" stroke-linejoin="round"{pattern}/>'
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


def gamma_density(x, shape, rate):
    if x < 0:
        return 0.0
    return rate**shape * x**(shape - 1) * exp(-rate * x) / gamma(shape)


def cdf(z):
    return (1 + erf(z / sqrt(2))) / 2


def key(diagram, left, y, name, *, stroke=INK, dash=None):
    diagram.line(left, y - 7, left + 40, y - 7, stroke=stroke, dash=dash)
    diagram.label(left + 54, y, name, math=True, anchor="start", size=26)


def density_area():
    d = Diagram()
    plot = Plot(d, (94, 75, 878, 269), (0, 8), 0.33)
    # A compact density with unit area and a finite support.
    def density(x):
        return 6 * x * (8 - x) / 8**3

    plot.area(density, 0, 8, "#f4f7fb")
    plot.area(density, 2, 5)
    plot.curve(density)
    plot.frame("x", "f(x)", ((2, "a"), (5, "b")))
    d.label(plot.x(3.5), 215, "P(a ≤ X ≤ b)", math=True, size=29)
    d.label(690, 62, "曲線下の全面積 = 1", size=26)
    d.write(CONTINUOUS, "density-interval-area", 320, "密度と区間確率")


def exponential_area():
    d = Diagram()
    plot = Plot(d, (102, 72, 878, 272), (0, 6), 1.07)
    def density(x):
        return gamma_density(x, 1, 1)

    plot.area(density, 0, 1.8)
    plot.curve(density)
    plot.frame("x", "f(x)", (0, (1.8, "t")), ((1, "λ"),))
    d.label(655, 103, "Ex(λ)", math=True, size=29)
    d.label(588, 173, "F(t) = P(X ≤ t)", math=True, size=29)
    d.line(475, 179, plot.x(1.08), plot.y(0.23), arrow=True)
    d.write(CONTINUOUS, "exponential-cumulative-area", 322, "指数分布の累積確率")


def gamma_shapes():
    d = Diagram()
    plot = Plot(d, (90, 73, 878, 272), (0, 13), 1.08)
    for shape, color, dash, legend in (
        (1, BLUE, None, 85), (3, RED, "11 5", 120), (6, INK, "3 5", 155)
    ):
        plot.curve(lambda x: gamma_density(x, shape, 1), stroke=color, dash=dash)
        key(d, 686, legend, f"Ga({shape}, 1)", stroke=color, dash=dash)
    plot.frame("x", "f(x)", (0, 2, 4, 6, 8, 10, 12), (0.5, 1))
    d.write(CONTINUOUS, "gamma-shapes", 322, "率を固定したガンマ分布")


def skewness():
    d = Diagram()
    # A standardized gamma distribution and its reflection.
    def right_tail(z):
        return sqrt(2) * gamma_density(2 + sqrt(2) * z, 2, 1)

    for index, sign, title in ((0, 1, "右に長い裾   α₃ > 0"),
                               (1, -1, "左に長い裾   α₃ < 0")):
        left = 69 + 457 * index
        plot = Plot(d, (left, 110, left + 350, 248), (-5, 5), 0.59)
        density = lambda z: right_tail(sign * z)
        plot.area(density, -5, 0, RED_FILL)
        plot.area(density, 0, 5)
        plot.curve(density)
        plot.frame("z", "f(z)", (0,))
        d.label(left + 175, 38, title, size=28)
        d.label(left + 62, 211, "z³ < 0", math=True)
        d.label(left + 285, 211, "z³ > 0", math=True)
    d.write(CONTINUOUS, "skewness-signed-contributions", 295, "歪度の符号と裾")


def kurtosis():
    d = Diagram()
    left = Plot(d, (65, 116, 420, 281), (-6, 6), 0.76)
    right = Plot(d, (535, 116, 890, 281), (-6, 6), 1.02)
    def laplace(z):
        return exp(-sqrt(2) * abs(z)) / sqrt(2)

    for name, density, color, dash in (
        ("正規分布   α₄ = 3", gaussian(0, 1), BLUE, None),
        ("ラプラス分布   α₄ = 6", laplace, RED, "9 5"),
    ):
        left.curve(density, stroke=color, dash=dash)
        right.curve(lambda z: z**4 * density(z), stroke=color, dash=dash)
        key(d, 115 if dash is None else 518, 37, name, stroke=color, dash=dash)
    left.frame("z", "f(z)", (-4, 0, 4))
    right.frame("z", "z⁴ f(z)", (-4, 0, 4))
    d.write(CONTINUOUS, "kurtosis-tail-contributions", 332, "4 次モーメントへの寄与")


def square_increment():
    d = Diagram()
    x, y, side, delta = 518, 52, 244, 98
    d.rect(80, y, side, side, "#f4f7fb")
    d.label(80 + side / 2, 27, "x", math=True, size=29)
    d.line(83, 36, 80 + side - 3, 36, stroke=GUIDE)
    d.label(80 + side / 2, y + side / 2 + 10, "x²", math=True, size=34)
    d.line(367, y + side / 2, 470, y + side / 2, arrow=True)
    d.rect(x, y, side, side, "#f4f7fb")
    d.rect(x + side, y, delta, side, BLUE_FILL)
    d.rect(x, y + side, side, delta, BLUE_FILL)
    d.rect(x + side, y + side, delta, delta, RED_FILL)
    for px, py, label in (
        (x + side / 2, y + side / 2, "x²"),
        (x + side + delta / 2, y + side / 2, "x Δx"),
        (x + side / 2, y + side + delta / 2, "x Δx"),
        (x + side + delta / 2, y + side + delta / 2, "(Δx)²"),
    ):
        d.label(px, py + 10, label, math=True, size=34)
    for start, end, center, label in (
        (x, x + side, x + side / 2, "x"),
        (x + side, x + side + delta, x + side + delta / 2, "Δx"),
    ):
        d.line(start + 3, 36, end - 3, 36, stroke=GUIDE)
        d.label(center, 27, label, math=True, size=29)
    d.write(CONTINUOUS, "square-area-increment", 418, "正方形の面積の増分")


def normal_parameters():
    d = Diagram()
    plot = Plot(d, (75, 92, 879, 276), (-5, 6), 0.46)
    for mean, sigma, name, color, dash, left in (
        (0, 1, "N(0, 1)", BLUE, None, 84),
        (2, 1, "N(2, 1)", RED, "11 5", 321),
        (0, 1.5, "N(0, 1.5²)", INK, "3 5", 562),
    ):
        plot.curve(gaussian(mean, sigma), stroke=color, dash=dash)
        key(d, left, 35, name, stroke=color, dash=dash)
    plot.frame("x", "f(x)", (-4, -2, 0, 2, 4, 6), (0.2, 0.4))
    d.write(NORMAL, "normal-mean-and-spread", 325, "平均と標準偏差による正規密度の違い")


def measurement_error():
    d = Diagram()
    plot = Plot(d, (85, 70, 873, 266), (-4, 4), 0.46)
    density = gaussian(0, 1)
    plot.area(density, -0.55, 0, RED_FILL)
    plot.area(density, 0, 0.55)
    plot.curve(density)
    plot.guide(0, density(0))
    plot.frame("x", "f(x)", ((0, "真の値 = μ"),))
    d.label(269, 204, "負のずれ", size=28)
    d.label(693, 204, "正のずれ", size=28)
    d.write(NORMAL, "measurements-around-true-value", 320, "正規分布で表した測定値と真の値")


def exam_ranges():
    d = Diagram()
    center = 450
    unit = 61
    for width, row, exam in ((10, 91, "A"), (8, 223, "B")):
        half_width = width * unit / 2
        left, right = center - half_width, center + half_width
        score = center + 3 * unit
        d.label(27, row + 8, f"試験 {exam}", anchor="start", size=28)
        d.rect(left, row - 10, 2 * half_width, 20, "#f4f7fb")
        d.rect(center, row - 10, 3 * unit, 20, BLUE_FILL)
        d.line(center, row - 22, center, row + 21)
        d.line(score, row - 22, score, row + 21)
        d.label(center, row - 32, "平均")
        d.label(score, row - 32, "得点")
        d.label((score + center) / 2, row + 43, "3 点")
        d.label(930, row + 8, f"幅 {width} 点", anchor="end", size=28)
    d.write(NORMAL, "exam-scores-and-ranges", 281, "平均から同じ点差にある得点")


def centering_and_scaling():
    d = Diagram()
    scores = (1, 3, 5)
    mean = sum(scores) / len(scores)
    centered = tuple(score - mean for score in scores)
    deviation = sqrt(sum(value**2 for value in centered) / len(centered))
    standardized = tuple(value / deviation for value in centered)
    left = 230
    unit = 53
    def position(x):
        return left + unit * (x + 4)

    for row, name, points, center in (
        (68, "観測値", scores, mean),
        (176, "平均を引く", centered, 0),
        (284, "標準偏差で割る", standardized, 0),
    ):
        d.label(20, row + 7, name, size=26, anchor="start")
        d.line(left, row, position(7), row, arrow=True)
        d.line(position(center), row - 23, position(center), row - 8, stroke=GUIDE)
        for point in points:
            d.dot(position(point), row)
            label = number(round(point, 2)).removesuffix(".0")
            x_tick(d, position(point), row, label)
    d.write(NORMAL, "centering-and-scaling", 333, "観測値の平行移動と尺度の変更")


def normal_panel(d, box, mean, sigma, ticks, regions, name, *, standard=False,
                 marker=None):
    left, top, right, _ = box
    domain = (mean - 3.8 * sigma, mean + 3.8 * sigma)
    plot = Plot(d, box, domain, 0.44 / sigma)
    density = gaussian(mean, sigma)
    for low, high in regions:
        plot.area(density, max(low, domain[0]), min(high, domain[1]))
    plot.curve(density)
    if marker is not None:
        plot.guide(marker, density(marker))
    plot.frame("z" if standard else "x", "φ(z)" if standard else "f(x)",
               ticks)
    d.label((left + right) / 2, top - 20, name, math=True, size=27)
    return plot


def probability_regions():
    d = Diagram()
    for index, (title, ticks, regions) in enumerate((
        ("下側確率", (0, (0.9, "z")), ((-4, 0.9),)),
        ("上側確率", (0, (0.9, "z")), ((0.9, 4),)),
        ("区間確率", ((-1, "a"), (1, "b")), ((-1, 1),)),
        ("両側確率", ((-1.2, "−z"), (1.2, "z")), ((-4, -1.2), (1.2, 4))),
    )):
        left = 58 + 458 * (index % 2)
        top = 65 + 212 * (index // 2)
        normal_panel(d, (left, top, left + 340, top + 117), 0, 1, ticks, regions,
                     title, standard=True)
    d.write(NORMAL, "normal-probability-regions", 445, "下側・上側・区間・両側確率")


def normal_table_area():
    d = Diagram()
    plot = normal_panel(d, (80, 78, 880, 266), 0, 1, (0, (1.5, "1.50")),
                        ((0, 1.5),), "N(0, 1)", standard=True)
    d.label(730, 133, f"Φ(1.50) − 0.5 ≈ {cdf(1.5) - 0.5:.4f}", math=True, size=28)
    d.line(672, 146, plot.x(0.85), plot.y(0.14), arrow=True)
    d.write(NORMAL, "standard-normal-table-area", 315, "標準正規分布の 0 から 1.50")


def lower_tail_standardized():
    d = Diagram()
    for left, mean, sigma, bound, title in (
        (58, 12, 2, 9, "N(12, 2²)"), (516, 0, 1, -1.5, "N(0, 1)")
    ):
        plot = normal_panel(d, (left, 64, left + 340, 231), mean, sigma,
                            (bound, mean), ((float("-inf"), bound),), title,
                            standard=sigma == 1)
        d.line(left + 49, 161, plot.x(mean - 2 * sigma), plot.y(0.022 / sigma),
               arrow=True)
        d.label(left + 46, 149, f"{cdf(-1.5):.4f}", size=27, math=True)
    d.write(NORMAL, "normal-lower-tail-standardization", 281, "同じ下側確率の標準化")


def interval_symmetry():
    d = Diagram()
    for index, (mean, sigma, bounds, title) in enumerate((
        (12, 2, (11, 12), "N(12, 2²)"),
        (0, 1, (-0.5, 0), "N(0, 1)"),
        (0, 1, (0, 0.5), "N(0, 1)"),
        (12, 2, (15, float("inf")), "N(12, 2²)"),
    )):
        left = 47 + 464 * (index % 2)
        top = 64 + 224 * (index // 2)
        interval = bounds[1] != float("inf")
        ticks = () if interval else (12, 15)
        plot = normal_panel(d, (left, top, left + 376, top + 130), mean, sigma,
                            ticks, (bounds,), title, standard=sigma == 1)
        if interval:
            x_tick(d, plot.x(bounds[0]), plot.bottom, number(bounds[0]), anchor="end")
            x_tick(d, plot.x(bounds[1]), plot.bottom, number(bounds[1]),
                   anchor="start")
            probability = cdf(0.5) - 0.5
            midpoint = sum(bounds) / 2
            label_x = left + (299 if bounds[0] < mean else 80)
            pointer_y = plot.y(0.24 / sigma)
        else:
            probability = cdf(-1.5)
            midpoint = 15.8
            label_x = left + 315
            pointer_y = plot.y(0.026 / sigma)
        d.label(label_x, top + 60, f"{probability:.4f}", math=True, size=26)
        d.line(label_x, top + 70, plot.x(midpoint), pointer_y, arrow=True)
    d.write(NORMAL, "normal-interval-symmetry", 470, "区間の標準化と対称性、上側確率")


def exercise_interval():
    d = Diagram()
    plot = normal_panel(d, (91, 87, 876, 272), 8, 2, (6, 8, 10), ((6, 8),),
                        "N(8, 2²)")
    d.label(329, 118, "6 ≤ X ≤ 8", math=True, size=31)
    d.line(340, 126, plot.x(7), plot.y(0.12), arrow=True)
    d.write(NORMAL, "normal-exercise-interval", 323, "確率変数が 6 から 8 までに入る範囲")


def waiting_time_means():
    d = Diagram()
    rng = Random(WAIT_SEED)
    mean = 5
    bin_width = 0.5
    bin_count = 40
    for index, size in enumerate((1, 4, 16)):
        left = 54 + index * 303
        plot = Plot(d, (left, 95, left + 234, 267), (0, 20), 0.42)
        counts = [0] * bin_count
        for _ in range(WAIT_TRIALS):
            average = sum(rng.expovariate(1 / mean) for _ in range(size)) / size
            bin_index = int(average / bin_width)
            if bin_index < bin_count:
                counts[bin_index] += 1
        for bin_index, count in enumerate(counts):
            if count:
                height = count / (WAIT_TRIALS * bin_width)
                x = plot.x(bin_index * bin_width)
                d.rect(x, plot.y(height), plot.x(bin_width) - left,
                       plot.bottom - plot.y(height), BLUE_FILL)
        plot.guide(mean, 0.41)
        plot.frame("", "密度", (0, 5, 10, 15, 20), (0.2, 0.4))
        if size == 1:
            d.label(left + 117, 34, "1 人の待ち時間", size=26)
        else:
            d.label(left + 101, 34, f"{size} 人の標本平均", size=26)
            x = left + 215
            d.label(x, 34, "X", math=True, size=26)
            d.line(x - 7, 13, x + 8, 13)
            d.label(x + 10, 40, "n", math=True, size=22, anchor="start")
    d.label(480, 332, "待ち時間 (分)", size=25)
    d.write(CLT, "waiting-time-sample-means", 350, "乱数で生成した待ち時間と標本平均")


def binomial_approximation():
    d = Diagram()
    n, p = 40, 0.5
    mean = n * p
    variance = n * p * (1 - p)
    plot = Plot(d, (90, 95, 846, 294), (-1, n + 1), 0.145)
    for k in range(n + 1):
        mass = comb(n, k) * p**k * (1 - p)**(n - k)
        left = plot.x(k - 0.5)
        d.rect(left, plot.y(mass), plot.x(k + 0.5) - left,
               plot.bottom - plot.y(mass), BLUE_FILL)
    plot.curve(gaussian(mean, sqrt(variance)), stroke=RED)
    plot.frame("", "確率質量・密度", (0, 10, 20, 30, 40), (0.05, 0.10))
    d.label(468, 362, "成功回数", size=25)
    d.rect(190, 20, 39, 20, BLUE_FILL)
    d.label(244, 38, "Bi(40, 1/2)", math=True, anchor="start", size=27)
    key(d, 526, 38, "N(20, 10)", stroke=RED)
    d.write(CLT, "binomial-normal-approximation", 381, "二項分布と正規近似")


def coin_upper_tail():
    d = Diagram()
    p = erfc(2 / sqrt(2)) / 2
    for left, mean, sigma, bound, title in (
        (58, 5000, 50, 5100, "N(5000, 2500)"), (516, 0, 1, 2, "N(0, 1)")
    ):
        plot = normal_panel(d, (left, 66, left + 340, 235), mean, sigma, (mean, bound),
                            ((bound, float("inf")),), title, standard=sigma == 1)
        d.label(left + 276, 151, f"{p:.4f}", math=True, size=27)
        d.line(left + 276, 162, plot.x(mean + 2.25 * sigma),
               plot.y(0.015 / sigma), arrow=True)
    d.write(CLT, "coin-count-upper-tail", 282, "コイン 10000 回の表の回数と上側確率")


def main():
    density_area()
    exponential_area()
    gamma_shapes()
    skewness()
    kurtosis()
    square_increment()
    normal_parameters()
    measurement_error()
    exam_ranges()
    centering_and_scaling()
    probability_regions()
    normal_table_area()
    lower_tail_standardized()
    interval_symmetry()
    exercise_interval()
    waiting_time_means()
    binomial_approximation()
    coin_upper_tail()


if __name__ == "__main__":
    main()
