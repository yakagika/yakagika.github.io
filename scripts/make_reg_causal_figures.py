"""合成 GPA の因果グラフを描き, 観測 GPA の回帰係数と介入の平均差を出力する."""

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import statsmodels.api as sm
from matplotlib.axes import Axes
from matplotlib.patches import FancyArrowPatch, FancyBboxPatch

from make_reg_gpa import draw_sample, intervene

OUTPUT = (
    Path(__file__).resolve().parents[1]
    / "images/fields/regression/reg-causal/gpa-causal-graphs.png"
)
SAMPLE_SIZE = 200_000
SEED = 2026
STUDY_SHIFT = 1
STUDY = "Study_Hours"
WORK = "Part_time_Work"
SCHOLARSHIP = "Scholarship"


def study_slope(sample: pd.DataFrame, predictors: list[str]) -> float:
    """観測 GPA を目的変数とする切片つき最小二乗法で勉強時間の係数を求める."""
    design = sm.add_constant(sample[predictors].astype(float))
    return float(sm.OLS(sample["GPA"], design).fit().params[STUDY])


def print_comparisons() -> None:
    """各構造について同じ学生の介入前後を比べ, 本文の表を出力する."""
    print(f"n = {SAMPLE_SIZE:,} per structure, seed = {SEED}, shift = {STUDY_SHIFT} h/week")
    print(
        "| アルバイト時間の役割 | 入れた式の係数 | 入れない式の係数 "
        "| 介入の平均差 (潜在得点) | 介入の平均差 (観測 GPA) |"
    )
    print("|---|---:|---:|---:|---:|")
    for structure, label in (("mediator", "中間の変数"), ("confounder", "交絡因子")):
        sample = draw_sample(np.random.default_rng(SEED), SAMPLE_SIZE, structure)
        with_work = study_slope(sample, [SCHOLARSHIP, STUDY, WORK])
        without_work = study_slope(sample, [SCHOLARSHIP, STUDY])
        # 同じシードで, 回帰に用いた架空の学生の介入前後の差を求める.
        latent, observed = intervene(
            np.random.default_rng(SEED), SAMPLE_SIZE, structure, STUDY_SHIFT
        )
        print(
            f"| {label} | {with_work:.3f} | {without_work:.3f} "
            f"| {latent:.3f} | {observed:.3f} |"
        )


def draw_graph(ax: Axes, title: str, study_work_edge: tuple[str, str]) -> None:
    """勉強時間とアルバイト時間の間の矢印を指定して因果グラフを描く."""
    positions = {
        "Study hours": (0.19, 0.20),
        "Part-time work": (0.50, 0.78),
        "GPA": (0.81, 0.20),
    }
    boxes = {}
    for label, (x, y) in positions.items():
        is_work = label == "Part-time work"
        box = FancyBboxPatch(
            (x - 0.164, y - 0.095),
            0.328,
            0.19,
            boxstyle="round,pad=0.008,rounding_size=0.035",
            facecolor="#edf6fc" if is_work else "#f7f8fa",
            edgecolor="#0072b2" if is_work else "#495363",
            linewidth=1.3,
        )
        ax.add_patch(box)
        boxes[label] = box
        ax.text(x, y, label, ha="center", va="center", fontsize=12, color="#172232")

    edges = (("Study hours", "GPA"), ("Part-time work", "GPA"), study_work_edge)
    for source, target in edges:
        arrow = FancyArrowPatch(
            positions[source],
            positions[target],
            patchA=boxes[source],
            patchB=boxes[target],
            shrinkA=3,
            shrinkB=3,
            arrowstyle="-|>",
            mutation_scale=16,
            linewidth=1.6,
            color="#0072b2" if (source, target) == study_work_edge else "#495363",
            zorder=2,
        )
        ax.add_patch(arrow)

    ax.set(xlim=(0, 1), ylim=(0, 1))
    ax.set_title(title, fontsize=14, pad=8)
    ax.set_axis_off()


def main() -> None:
    print_comparisons()

    plt.switch_backend("Agg")
    fig, (left, right) = plt.subplots(1, 2, figsize=(10, 2.75), layout="constrained")
    fig.get_layout_engine().set(w_pad=0.12, h_pad=0.12, wspace=0.06)
    draw_graph(left, "(A) Mediator", ("Study hours", "Part-time work"))
    draw_graph(right, "(B) Confounder", ("Part-time work", "Study hours"))
    OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(OUTPUT, dpi=200, facecolor="white")
    plt.close(fig)


if __name__ == "__main__":
    main()
