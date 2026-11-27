#!/usr/bin/env python3
"""Generate population and point estimation figures and double-resolution PNGs."""

from math import log10, sqrt
from random import Random

from make_prob_sampling_svgs import (
    BLUE, BLUE_FILL, GUIDE, IMAGE_ROOT, INK, RED, RED_FILL,
    Diagram, Plot, chi_density, gaussian,
)

POPULATION = IMAGE_ROOT / "inference" / "inf-population"
POINT = IMAGE_ROOT / "inference" / "inf-point"
COIN_SEED = 9024
COIN_TRIALS = 4096


def statistics_roles():
    d = Diagram()
    d.box(21, 86, 225, 82, "データの収集設計", "対象・測定項目")
    d.box(331, 86, 156, 82, "観測の記録", fill="#fff")
    d.box(597, 23, 335, 76, "記述統計", "記録の分布・平均")
    d.box(597, 150, 335, 76, "推測統計", "母集団の平均の推定", fill=RED_FILL)
    d.line(250, 127, 326, 127, arrow=True)
    d.label(288, 115, "収集", size=22)
    for y in (61, 188):
        d.path([(492, 127), (541, 127), (541, y)], width=2)
        d.line(541, y, 592, y, arrow=True)
    d.write(POPULATION, "statistics-roles", 248, "データの収集設計・記述統計・推測統計")


def sampling_and_inference():
    d = Diagram()
    d.box(55, 28, 257, 83, "母集団", "森の全頭")
    d.box(648, 28, 257, 83, "標本", "選んだ個体の体重")
    d.box(55, 167, 257, 83, "母集団の特徴", "森全体の平均体重", fill=RED_FILL)
    d.box(648, 167, 257, 83, "標本の特徴", "選んだ個体の平均体重")
    d.line(318, 70, 642, 70, arrow=True)
    d.label(480, 57, "標本抽出")
    d.line(776.5, 116, 776.5, 162, arrow=True)
    d.label(795, 146, "要約", anchor="start")
    d.line(642, 209, 318, 209, arrow=True)
    d.label(480, 195, "推測")
    d.write(POPULATION, "sampling-and-inference", 277, "母集団と標本の間の抽出と推測")


def population_sample_statistics():
    d = Diagram()
    centers = (439, 753)
    d.rect(282, 22, 314, 173, BLUE_FILL)
    d.rect(596, 22, 314, 173, RED_FILL)
    for y, title in ((101, "個体・観測値の数"), (137, "平均"), (173, "分散")):
        d.label(249, y, title, anchor="end")
    for x, title, values in zip(
        centers, ("母集団", "標本"), (("N", "μ", "σ²"), ("n", "X̄ₙ", "S²"))
    ):
        d.label(x, 58, title)
        for y, value in zip((101, 137, 173), values):
            d.label(x, y, value, math=True, size=27)
    for y in (72, 110, 146):
        d.line(282, y, 910, y, stroke=GUIDE)
    d.write(POPULATION, "population-and-sample-statistics", 218, "母集団と標本の数・平均・分散")


def three_observations():
    d = Diagram()
    d.rect(45, 24, 340, 218, BLUE_FILL)
    d.label(215, 60, "母集団 (森の全頭)")
    chosen = {(0, 1): "1", (1, 3): "2", (2, 0): "3"}
    for row in range(3):
        for column in range(4):
            x, y = 106 + column * 70, 100 + row * 56
            label = chosen.get((row, column))
            d.dot(x, y, radius=17, fill=BLUE if label else "#fff")
            if label:
                d.label(x, y + 8, label, size=23, fill="#fff")
    d.line(391, 131, 566, 131, arrow=True)
    d.label(479, 115, "3 頭を選ぶ")
    d.rect(573, 24, 342, 218, RED_FILL)
    d.label(744, 60, "体重の標本 1 組")
    for i, (x, symbol) in enumerate(zip((658, 744, 830), ("x₁", "x₂", "x₃")), start=1):
        d.dot(x, 124, radius=16, fill=BLUE)
        d.label(x, 132, str(i), size=23, fill="#fff")
        d.label(x, 173, symbol, math=True)
    d.label(744, 214, "標本サイズ n = 3")
    d.write(POPULATION, "three-observations", 267, "森のウサギから 3 頭を選ぶ模式図")


def coin_frequencies():
    d = Diagram()
    p = Plot(d, (98, 65, 839, 334), (0, log10(COIN_TRIALS)), 1)
    d.line(p.left, p.y(0.5), p.right, p.y(0.5), stroke=INK, dash="9 6")
    random = Random(COIN_SEED)
    heads = 0
    points = []
    for n in range(1, COIN_TRIALS + 1):
        heads += random.random() < 0.5
        points.append((p.x(log10(n)), p.y(heads / n)))
    d.path(points, stroke=BLUE, width=2.6)
    ticks = [(log10(n), str(n)) for n in (1, 10, 100, 1000, COIN_TRIALS)]
    p.frame("", "表の累積相対度数", ticks, (0, 0.25, (0.5, "1/2"), 0.75, 1))
    d.label(891, p.y(0.5) + 7, "p = 1/2", math=True)
    d.label(475, 401, "試行回数 n (対数目盛り)")
    d.write(POPULATION, "coin-relative-frequency", 423, "公平なコインの表の累積相対度数")


def sample_mean_concentration():
    d = Diagram()
    for index, n in enumerate((4, 16, 64)):
        left = 52 + index * 310
        p = Plot(d, (left, 61, left + 237, 245), (-0.42, 0.42), 8.5)
        density = gaussian(0, 0.4 / sqrt(n))
        p.area(density, -0.1, 0.1)
        p.curve(density)
        for sign in (-1, 1):
            p.guide(sign * 0.1, 7.5)
        p.frame("", "確率密度" if index == 0 else "", ((0, "μ"),),
                (0, 2, 4, 6, 8) if index == 0 else ())
        for value, label, anchor in ((-0.1, "μ − ε", "end"), (0.1, "μ + ε", "start")):
            x = p.x(value)
            d.line(x, p.bottom, x, p.bottom + 7)
            d.label(x, p.bottom + 32, label, anchor=anchor, math=True)
        d.label(left + 190, 92, f"n = {n}", math=True)
    d.label(478, 326, "標本平均 X̄ₙ (kg)     ε = 0.1 kg")
    d.write(POPULATION, "sample-mean-concentration", 348, "標本サイズと母平均のまわりの確率")


def parameter_and_estimate():
    d = Diagram()
    x = (24, 332, 657)
    w = (212, 228, 279)
    for left, width, title, detail in zip(
        x, w, ("母数", "推定量", "推定値"),
        ("θ", "θ̂ = T(X₁, …, Xₙ)", "T(x₁, …, xₙ)"),
    ):
        d.box(left, 116, width, 92, title, detail)
    d.box(332, 15, 228, 57, "標本 X₁, …, Xₙ", fill="#fff")
    d.line(446, 77, 446, 111, arrow=True)
    d.line(562, 162, 651, 162, arrow=True)
    d.label(606, 150, "観測", size=22)
    d.line(328, 162, 240, 162, arrow=True)
    d.label(284, 150, "推定", size=22)
    d.write(POINT, "parameter-estimator-and-estimate", 231, "母数・推定量・推定値")


def population_and_sample_means():
    d = Diagram()
    for x, header, detail, title, formula in (
        (57, "母集団分布", "X", "母平均", "μ = E(X)"),
        (636, "標本", "X₁, …, Xₙ", "標本平均", "X̄ₙ = (X₁ + ⋯ + Xₙ)/n"),
    ):
        d.box(x, 20, 267, 76, header, detail, fill="#fff")
        d.line(x + 133.5, 100, x + 133.5, 131, arrow=True)
        d.box(x, 136, 267, 86, title, formula)
    d.line(630, 179, 330, 179, arrow=True)
    d.label(480, 166, "母平均を推定")
    d.label(480, 207, "E(X̄ₙ) = μ", math=True)
    d.write(POINT, "population-and-sample-means", 248, "母平均と標本平均の対応")


def variance_denominators():
    d = Diagram()
    d.box(33, 109, 255, 84, "標本の偏差平方和", "Σ (Xᵢ − X̄ₙ)²", fill="#fff")
    d.box(425, 23, 200, 83, "標本分散", "S²")
    d.box(425, 190, 200, 83, "不偏分散", "u²")
    d.box(718, 190, 200, 83, "母分散", "σ²", fill=RED_FILL)
    for y, label in ((64, "÷ n"), (232, "÷ (n − 1)")):
        d.path([(293, 151), (319, 151), (319, y)], width=2)
        d.line(319, y, 419, y, arrow=True)
        d.label(369, y - 10, label, size=22, math=True)
    d.line(630, 232, 713, 232, arrow=True)
    d.label(671, 219, "期待値", size=22)
    d.write(POINT, "variance-denominators", 296, "母分散・標本分散・不偏分散")


def reference_value(plot, d, value, limit, label, *, anchor="middle", dash="6 5"):
    x = plot.x(value)
    d.line(x, plot.y(limit), x, plot.bottom, dash=dash)
    d.label(x, plot.bottom + 33, label, anchor=anchor, math=True)


def sampling_distributions():
    d = Diagram()
    n = 10
    mean = Plot(d, (53, 100, 418, 298), (-1.1, 1.1), 1.4)
    variance = Plot(d, (538, 100, 902, 298), (0, 2.6), 1.12)
    d.label(478, 32, "正規母集団 N(0,1) からの独立な標本 (n = 10)")
    mean.frame("X̄ₙ", "標本平均の確率密度")
    variance.frame("S²", "標本分散の確率密度")
    mean.curve(gaussian(0, 1 / sqrt(n)), stroke=BLUE)
    variance.curve(lambda x: n * chi_density(n * x, n - 1), stroke=BLUE)
    reference_value(mean, d, 0, 1.34, "μ = E(X̄ₙ)")
    reference_value(variance, d, (n - 1) / n, 1.05, "E(S²)", anchor="end")
    reference_value(variance, d, 1, 1.05, "σ²", anchor="start", dash="2 5")
    d.write(POINT, "mean-and-variance-sampling-distributions", 354,
            "正規標本の標本平均・標本分散の分布")


def consistency_and_efficiency():
    d = Diagram()
    consistent = Plot(d, (54, 80, 425, 289), (-3, 3), 1.8)
    efficient = Plot(d, (535, 80, 906, 289), (-3, 3), 1.8)
    for p, name in ((consistent, "一致性"), (efficient, "効率性")):
        p.frame("θ̂", "")
        d.label((p.left + p.right) / 2, 34, name)
        reference_value(p, d, 0, 1.65, "θ")
    for deviation, color, dash in ((1, RED, "11 5"), (0.25, BLUE, None)):
        consistent.curve(gaussian(0, deviation), stroke=color, dash=dash)
    for deviation, color, dash in ((1, RED, "11 5"), (0.45, BLUE, None)):
        efficient.curve(gaussian(0, deviation), stroke=color, dash=dash)
    for x, y, target, deviation, text in (
        (327, 151, 0.21, 0.25, "n = 64"),
        (357, 221, 1.10, 1.0, "n = 4"),
    ):
        d.line(x, y, consistent.x(target),
               consistent.y(gaussian(0, deviation)(target)), stroke=GUIDE)
        d.label(x + 8, y - 6, text, anchor="start", math=True)
    for x, y, target, deviation, text in (
        (802, 151, 0.35, 0.45, "分散が小さい"),
        (814, 223, 1.14, 1.0, "分散が大きい"),
    ):
        d.line(x, y, efficient.x(target),
               efficient.y(gaussian(0, deviation)(target)), stroke=GUIDE)
        d.label(x, y - 7, text)
    d.label(240, 365, "同じ推定量の系列")
    d.label(721, 365, "同じ n の 2 つの不偏推定量")
    d.write(POINT, "consistency-and-efficiency", 387, "一致性と効率性の比較")


def main():
    statistics_roles()
    sampling_and_inference()
    population_sample_statistics()
    three_observations()
    coin_frequencies()
    sample_mean_concentration()
    parameter_and_estimate()
    population_and_sample_means()
    variance_denominators()
    sampling_distributions()
    consistency_and_efficiency()


if __name__ == "__main__":
    main()
