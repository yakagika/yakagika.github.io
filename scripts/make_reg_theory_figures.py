"""合成 GPA の標本から回帰分析の理論に使う図と数値を再現する.

入力は GPA 生成器と固定した乱数シード. 図を images/fields/regression/ に保存し,
観測 GPA の係数と VIF, 潜在得点と観測 GPA の信頼区間の被覆割合を標準出力へ書く.
"""

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import statsmodels.api as sm

from make_reg_gpa import draw_sample

IMAGE_ROOT = Path(__file__).resolve().parents[1] / "images/fields/regression"
POPULATION_SIZE = 200_000
POPULATION_SEED = 2026
SAMPLE_SIZE = 100
LINE_SEEDS = (2028, 2029, 2030, 2031)
INTERVAL_SEED = 2027
INTERVAL_REPETITIONS = 1_000
INTERVALS_SHOWN = 60
ALPHA = 0.05
STRUCTURE = "mediator"
STUDY = "Study_Hours"
WORK = "Part_time_Work"
SCHOLARSHIP = "Scholarship"
POPULATION_COLOR = "#222222"
COVERED_COLOR = "#0072b2"
MISSED_COLOR = "#b13b00"


def fit_ols(
    sample: pd.DataFrame,
    response: str,
    predictors: list[str],
) -> sm.regression.linear_model.RegressionResultsWrapper:
    """指定した列を用い, 切片つき最小二乗法で当てはめる.

    sample は GPA 生成器が返す学生ごとの表. 時間の列の単位は週あたりの時間.
    奨学生の列を使う場合の符号は奨学生が 1, 非奨学生が 0.
    """
    design = sm.add_constant(sample[predictors].astype(float))
    return sm.OLS(sample[response], design).fit()


def draw_sample_lines(
    population_fit: sm.regression.linear_model.RegressionResultsWrapper,
    sample_fits: list[sm.regression.linear_model.RegressionResultsWrapper],
) -> None:
    """勉強時間で観測 GPA を当てはめた母集団近似と標本の直線を保存する."""
    hours = np.array([2, 18])
    fig, ax = plt.subplots(figsize=(7.2, 4.3), layout="constrained")
    styles = (
        ("#0072b2", (0, (6, 3))),
        ("#b13b00", (0, (3, 2))),
        ("#00765f", (0, (6, 2, 1, 2))),
        ("#8e4b8b", (0, (1, 2))),
    )
    population_line = (
        population_fit.params["const"] + population_fit.params[STUDY] * hours
    )
    ax.plot(
        hours,
        population_line,
        color=POPULATION_COLOR,
        linewidth=2.7,
        label=f"Population approximation (n = {POPULATION_SIZE:,})",
    )
    for index, (fit, (color, linestyle)) in enumerate(
        zip(sample_fits, styles, strict=True)
    ):
        line = fit.params["const"] + fit.params[STUDY] * hours
        ax.plot(
            hours,
            line,
            color=color,
            linestyle=linestyle,
            linewidth=1.9,
            label=f"Sample {index + 1} (n = {SAMPLE_SIZE})",
        )

    ax.set(
        xlabel="Study time (hours per week)", ylabel="Fitted GPA (points)", xlim=(2, 18)
    )
    ax.set_xticks(np.arange(2, 19, 2))
    ax.grid(color="#dedede", linewidth=0.7)
    ax.legend(loc="upper left", frameon=True, framealpha=1)
    output = IMAGE_ROOT / "reg-intro" / "sample-regression-lines.png"
    output.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(output, dpi=200, facecolor="white")
    plt.close(fig)


def calculate_intervals(response: str, reference: float) -> tuple[np.ndarray, np.ndarray]:
    """同じ乱数列で 100 人ずつ取り, 指定した応答の通常の t 区間を求める.

    各標本について, 週 1 時間あたりの得点の差の下限・上限と,
    区間が参照値を含むかどうかを返す.
    """
    rng = np.random.default_rng(INTERVAL_SEED)
    intervals = np.empty((INTERVAL_REPETITIONS, 2))
    for index in range(INTERVAL_REPETITIONS):
        sample = draw_sample(rng, SAMPLE_SIZE, STRUCTURE)
        fit = fit_ols(sample, response, [STUDY])
        intervals[index] = fit.conf_int(alpha=ALPHA).loc[STUDY]
    covers = (intervals[:, 0] <= reference) & (reference <= intervals[:, 1])
    return intervals, covers


def draw_intervals(intervals: np.ndarray, reference: float, covers: np.ndarray) -> None:
    """潜在得点の最初の区間を描き, 全標本での被覆数と割合を併記する."""
    lower, upper = intervals[:INTERVALS_SHOWN].T
    numbers = np.arange(1, INTERVALS_SHOWN + 1)
    shown_covers = covers[:INTERVALS_SHOWN]
    covered_count = np.count_nonzero(covers)

    fig, ax = plt.subplots(figsize=(7.2, 5.4), layout="constrained")
    ax.axvline(
        reference,
        color=POPULATION_COLOR,
        linewidth=1.5,
        linestyle="--",
        label=f"Latent-score reference slope ({reference:.3f})",
    )
    for covers_reference, color, label in (
        (True, COVERED_COLOR, "Contains reference slope"),
        (False, MISSED_COLOR, "Misses reference slope"),
    ):
        mask = shown_covers == covers_reference
        ax.hlines(
            numbers[mask],
            lower[mask],
            upper[mask],
            color=color,
            linewidth=1.8,
            label=label,
        )

    ax.set(
        title=f"Coverage: {covered_count:,}/{len(covers):,} ({covers.mean():.1%})",
        xlabel="Study-time slope for the latent score [points / (hour per week)]",
        ylabel="Sample number",
        ylim=(INTERVALS_SHOWN + 1, 0),
    )
    ax.set_yticks([1, 10, 20, 30, 40, 50, 60])
    ax.grid(axis="x", color="#dedede", linewidth=0.7)
    ax.legend(loc="upper center", bbox_to_anchor=(0.5, -0.14), frameon=False)
    output = IMAGE_ROOT / "reg-ols" / "confidence-interval-coverage.png"
    output.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(output, dpi=200, facecolor="white")
    plt.close(fig)


def main() -> None:
    plt.switch_backend("Agg")
    population = draw_sample(
        np.random.default_rng(POPULATION_SEED), POPULATION_SIZE, STRUCTURE
    )
    simple = fit_ols(population, "GPA", [STUDY])
    with_work = fit_ols(population, "GPA", [SCHOLARSHIP, STUDY, WORK])
    without_work = fit_ols(population, "GPA", [SCHOLARSHIP, STUDY])
    study_auxiliary = fit_ols(population, STUDY, [SCHOLARSHIP, WORK])
    study_vif = 1 / (1 - study_auxiliary.rsquared)

    sample_fits = [
        fit_ols(
            draw_sample(np.random.default_rng(seed), SAMPLE_SIZE, STRUCTURE),
            "GPA",
            [STUDY],
        )
        for seed in LINE_SEEDS
    ]
    draw_sample_lines(simple, sample_fits)
    latent_reference = fit_ols(population, "GPA_latent", [STUDY]).params[STUDY]
    observed_reference = simple.params[STUDY]
    latent_intervals, latent_covers = calculate_intervals("GPA_latent", latent_reference)
    _, observed_covers = calculate_intervals("GPA", observed_reference)
    draw_intervals(latent_intervals, latent_reference, latent_covers)
    clipped = population["GPA_latent"] != population["GPA"]

    print(f"Population approximation (n = {POPULATION_SIZE:,}, seed = {POPULATION_SEED})")
    print(f"Simple regression: intercept = {simple.params['const']:.6f}")
    print(f"Simple regression: slope = {observed_reference:.6f}")
    for seed, fit in zip(LINE_SEEDS, sample_fits, strict=True):
        print(f"Sample seed {seed}: slope = {fit.params[STUDY]:.6f}")
    for label, fit in (
        ("With work time", with_work),
        ("Without work time", without_work),
    ):
        print(label)
        print(fit.params.to_string(float_format="{:.6f}".format))
    print(f"Study-time VIF (with work time): {study_vif:.6f}")
    print(f"Study-time standard error multiplier: {np.sqrt(study_vif):.6f}")
    print(f"Population approximation: clipped GPA = {clipped.sum():,}/{len(clipped):,}")
    for label, reference, covers in (
        ("Latent score", latent_reference, latent_covers),
        ("Observed GPA", observed_reference, observed_covers),
    ):
        print(
            f"{label} (reference slope = {reference:.6f}): "
            f"coverage = {covers.sum():,}/{len(covers):,} ({covers.mean():.1%})"
        )


if __name__ == "__main__":
    main()
