"""合成 GPA の標本と説明用の合成データから, 回帰分析の理論に使う図と数値を再現する.

入力は GPA 生成器と固定した乱数シード. 図を images/fields/regression/ に保存し,
観測 GPA の係数と VIF, 潜在得点と観測 GPA の信頼区間の被覆割合を標準出力へ書く.
"""

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import statsmodels.api as sm
from matplotlib.axes import Axes
from matplotlib.figure import Figure

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


def save_teaching_figure(fig: Figure, block: str, filename: str) -> None:
    """新しい参考図を対応するブロックのディレクトリに保存する."""
    output = IMAGE_ROOT / block / filename
    output.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(output, dpi=200, facecolor="white")
    plt.close(fig)


def label_gpa_axes(ax: Axes, *, mean: bool = False) -> None:
    ax.set(
        xlabel="Study time (hours/week)",
        ylabel="Mean GPA (points)" if mean else "GPA (points)",
        ylim=(0, 4),
    )
    ax.grid(alpha=0.25)
    ax.set_axisbelow(True)


def draw_correlation_and_slopes(x: np.ndarray, deviations: np.ndarray) -> None:
    """縦軸の尺度だけが異なる二つの標本で相関係数と傾きを比べる."""
    fig, axes = plt.subplots(
        1, 2, figsize=(8.4, 4.0), sharex=True, sharey=True, layout="constrained"
    )
    for ax, scale in zip(axes, (0.30, 0.80), strict=True):
        y = scale * (x + 0.75 * deviations)
        slope, intercept = np.polyfit(x, y, 1)
        correlation = np.corrcoef(x, y)[0, 1]
        ax.scatter(x, y, color=COVERED_COLOR, s=28, zorder=3)
        ax.plot(x, intercept + slope * x, color=POPULATION_COLOR)
        ax.set_title(f"Correlation: {correlation:.2f}\nSlope: {slope:.2f}", fontsize=20)
        ax.set(xticks=[-1, 0, 1], ylim=(-2.2, 2.2))
        ax.set_xlabel(r"$x$", fontsize=15)
        ax.set_yticks([-2, -1, 0, 1, 2])
        if ax is axes[0]:
            ax.set_ylabel(r"$y$", fontsize=15)
        ax.tick_params(labelsize=13)
        ax.grid(alpha=0.25)
        ax.set_axisbelow(True)

    save_teaching_figure(fig, "reg-intro", "same-correlation-different-slopes.png")


def draw_regression_coefficients() -> None:
    """回帰直線の切片と横軸 1 単位分の傾きを示す."""
    fig, ax = plt.subplots(figsize=(6.6, 4.2), layout="constrained")
    hours = np.array([0, 12])
    intercept, slope = 0.5, 0.27
    ax.plot(hours, intercept + slope * hours, color=COVERED_COLOR, linewidth=2.5)
    ax.scatter([0], [intercept], color=POPULATION_COLOR, zorder=3, clip_on=False)
    ax.annotate(r"Intercept $\beta_0$", (0.3, 0.34), fontsize=13)
    ax.text(0.46, 0.87, r"$m(x)=\beta_0+\beta_1 x$", transform=ax.transAxes, fontsize=17)
    step_start, step_end = intercept + slope * np.array([8, 9])
    ax.plot(
        [8, 9, 9], [step_start, step_start, step_end], color=MISSED_COLOR, linewidth=2
    )
    ax.annotate(
        "1 hour/week",
        (8.5, step_start),
        (0, -20),
        textcoords="offset points",
        ha="center",
        fontsize=12,
    )
    ax.annotate(
        r"Slope $\beta_1$",
        (9, (step_start + step_end) / 2),
        (9, -3),
        textcoords="offset points",
        fontsize=12,
    )
    label_gpa_axes(ax, mean=True)
    ax.set_xlim(0, 12)

    save_teaching_figure(fig, "reg-intro", "regression-coefficients.png")


def draw_plane_and_curve() -> None:
    """説明変数 2 つの平面と係数が指数に入る曲線を並べて示す."""
    fig = plt.figure(figsize=(8.8, 4.1), layout="constrained")
    fig.get_layout_engine().set(wspace=0.14)
    plane = fig.add_subplot(121, projection="3d", computed_zorder=False)
    study_hours = np.linspace(8, 16, 9)
    work_hours = np.linspace(6, 21, 11)
    study, work = np.meshgrid(study_hours, work_hours)
    intercept, study_slope, work_slope = 0.82, 0.25, -0.13
    gpa = intercept + study_slope * study + work_slope * work
    plane.plot_surface(
        study,
        work,
        gpa,
        color="#a9d4ec",
        edgecolor="#6794aa",
        alpha=0.35,
        linewidth=0.55,
        shade=False,
    )

    sample_study = np.array([8.5, 10, 11, 13, 14, 15, 9.5, 12.5, 15.5, 13.5])
    sample_work = np.array([8, 9, 15, 18, 7, 13, 19, 11, 17, 20])
    deviations = np.array(
        [0.42, -0.33, 0.52, -0.40, -0.46, 0.45, 0.31, -0.35, 0.32, -0.25]
    )
    fitted_sample = intercept + study_slope * sample_study + work_slope * sample_work
    observed = fitted_sample + deviations
    for study_hour, work_hour, prediction, actual in zip(
        sample_study, sample_work, fitted_sample, observed, strict=True
    ):
        plane.plot(
            [study_hour, study_hour],
            [work_hour, work_hour],
            [prediction, actual],
            color="#657078",
            linewidth=0.8,
            zorder=2.6,
        )
    plane.scatter(
        sample_study,
        sample_work,
        observed,
        color=POPULATION_COLOR,
        edgecolor="white",
        linewidth=0.6,
        depthshade=False,
        s=26,
        zorder=4,
    )
    for fixed_work, color, linestyle in (
        (8, COVERED_COLOR, "-"),
        (16, MISSED_COLOR, "--"),
    ):
        fitted = intercept + study_slope * study_hours + work_slope * fixed_work
        plane.plot(
            study_hours,
            np.full_like(study_hours, fixed_work),
            fitted,
            color=color,
            linestyle=linestyle,
            linewidth=2.6,
            label=rf"$x_2={fixed_work}$ h/week",
            zorder=3,
        )

    plane.set(xticks=[8, 12, 16], yticks=[8, 16, 20], zticks=[0, 2, 4], zlim=(0, 4))
    plane.set_xlabel(r"Study $x_1$ (h/week)", fontsize=11, labelpad=5)
    plane.set_ylabel(r"Work $x_2$ (h/week)", fontsize=11, labelpad=5)
    plane.set_zlabel("GPA (points)", fontsize=11, labelpad=2)
    plane.tick_params(pad=0)
    plane.set_box_aspect((1.12, 1, 0.95))
    plane.set_proj_type("ortho")
    plane.view_init(elev=19, azim=140)
    plane.set_title(
        "Linear regression\n" r"$m(x_1,x_2)=\beta_0+\beta_1x_1+\beta_2x_2$",
        fontsize=15,
    )
    plane.legend(loc="upper left", fontsize=9, frameon=False, borderaxespad=0)

    curve = fig.add_subplot(122)
    hours = np.linspace(0, 24, 160)
    curve.plot(
        hours, 3.5 * (-np.expm1(-0.15 * hours)), color=COVERED_COLOR, linewidth=2.5
    )
    curve.axhline(3.5, color=POPULATION_COLOR, linestyle="--", linewidth=1)
    curve.text(1, 3.62, r"Upper limit $\beta_0$", fontsize=12)
    curve.text(2, 0.53, r"$m(x)=\beta_0\{1-\exp(-\beta_1 x)\}$", fontsize=14)
    curve.set_title("Nonlinear regression", fontsize=16, pad=11)
    label_gpa_axes(curve, mean=True)
    curve.set_xlim(0, 24)

    save_teaching_figure(fig, "reg-intro", "linear-plane-and-nonlinear-curve.png")


def draw_population_errors() -> None:
    """母回帰式からの誤差と, 等分散正規誤差を仮定した条件つき分布を示す."""
    fig, (error_ax, densities_ax) = plt.subplots(
        1, 2, figsize=(8.8, 4.2), sharey=True, layout="constrained"
    )
    hours = np.array([3, 6, 8, 10, 12, 14, 16])
    mean = 0.7 + 0.15 * hours
    observed = mean + np.array([0.30, -0.33, 0.22, 0.67, -0.38, 0.29, -0.31])
    for ax in (error_ax, densities_ax):
        ax.plot(hours, mean, color=POPULATION_COLOR, linewidth=1.8)
        label_gpa_axes(ax)
        ax.set(xlim=(2, 18), xticks=[4, 8, 12, 16])

    index = 3
    error_ax.scatter(hours, observed, color=COVERED_COLOR, s=25, zorder=3)
    error_ax.vlines(
        hours[index], mean[index], observed[index], color=MISSED_COLOR, lw=2.5
    )
    error_ax.annotate(r"$\varepsilon_i$", (10.3, 2.48), fontsize=17, color=MISSED_COLOR)
    error_ax.text(2.7, 0.32, r"$m(x)=E(Y\mid X=x)$", fontsize=15)
    error_ax.set_title("Error for one observation", fontsize=15)

    offsets = np.linspace(-0.9, 0.9, 140)
    width = 2.2 * np.exp(-0.5 * (offsets / 0.27) ** 2)
    for hour in (5, 10, 15):
        center = 0.7 + 0.15 * hour
        densities_ax.fill_betweenx(
            center + offsets, hour, hour + width, color="#cde5f3", zorder=3
        )
        densities_ax.plot(hour + width, center + offsets, color=COVERED_COLOR, zorder=4)
        densities_ax.scatter(hour, center, color=POPULATION_COLOR, s=20, zorder=5)
    densities_ax.set_ylabel("")
    densities_ax.set_title(r"Normal errors $N(0,\sigma^2)$", fontsize=15)

    save_teaching_figure(fig, "reg-ols", "population-errors.png")


def draw_least_squares_deviations(hours: np.ndarray, observed: np.ndarray) -> None:
    """候補の直線について, 縦方向の符号つきの差を示す."""
    fig, ax = plt.subplots(figsize=(6.6, 4.2), layout="constrained")
    candidate = 0.4 + 0.17 * hours
    ax.plot(hours, candidate, color=POPULATION_COLOR, label=r"Candidate line $a+bx$")
    ax.vlines(
        hours,
        candidate,
        observed,
        color=MISSED_COLOR,
        linewidth=2,
        label=r"Vertical difference $y_i-a-bx_i$",
    )
    ax.scatter(
        hours, observed, color=COVERED_COLOR, zorder=3, label=r"Observation $(x_i,y_i)$"
    )
    ax.legend(loc="upper left")
    label_gpa_axes(ax)

    save_teaching_figure(fig, "reg-ols", "least-squares-deviations.png")


def draw_variation_decomposition(hours: np.ndarray, observed: np.ndarray) -> None:
    """1 観測の差の内訳と, 全観測における向きの違いを示す."""
    slope, intercept = np.polyfit(hours, observed, 1)
    fitted = intercept + slope * hours
    average = observed.mean()
    fig, ax = plt.subplots(figsize=(6.6, 4.2), layout="constrained")
    x_i, y_i, fitted_i = hours[-1], observed[-1], fitted[-1]
    ax.axhline(average, color="#7a7a7a", linestyle=":", label=r"Mean $\bar y$")
    ax.plot(hours, fitted, color=POPULATION_COLOR, label="Fitted line")
    ax.scatter(hours, observed, color=POPULATION_COLOR, s=22, zorder=4)
    ax.vlines(x_i, 0, y_i, color="#7a7a7a", linestyle=":", linewidth=1)
    ax.hlines(
        [fitted_i, y_i], x_i, x_i + 0.95, color="#7a7a7a", linestyle=":", linewidth=1
    )
    for offset, lower, upper, color, label in (
        (0.30, average, y_i, POPULATION_COLOR, r"$y_i-\bar y$"),
        (0.59, average, fitted_i, COVERED_COLOR, r"$\hat y_i-\bar y$"),
        (0.88, fitted_i, y_i, MISSED_COLOR, r"$e_i=y_i-\hat y_i$"),
    ):
        ax.vlines(x_i + offset, lower, upper, color=color, linewidth=2.8, label=label)
    ax.annotate(
        r"$(x_i,y_i)$",
        (x_i, y_i),
        (-6, 7),
        textcoords="offset points",
        ha="right",
        fontsize=12,
    )
    label_gpa_axes(ax)
    ax.set(xlim=(2, 18.6), xticks=[4, 8, 12, 16, x_i])
    ax.set_xticklabels(["4", "8", "12", "16", r"$x_i$"])
    ax.legend(loc="upper left", fontsize=10.5)
    save_teaching_figure(fig, "reg-ols", "decomposition-one-observation.png")

    fig, ax = plt.subplots(figsize=(6.6, 4.2), layout="constrained")
    ax.axhline(average, color="#7a7a7a", linestyle=":", label=r"Mean $\bar y$")
    ax.plot(hours, fitted, color=POPULATION_COLOR, label="Fitted line")
    ax.vlines(
        hours - 0.15,
        average,
        fitted,
        color=COVERED_COLOR,
        linewidth=2.8,
        label=r"Fitted $-$ mean ($\hat y_i-\bar y$)",
    )
    ax.vlines(
        hours + 0.15,
        fitted,
        observed,
        color=MISSED_COLOR,
        linewidth=2.8,
        label=r"Residual ($e_i$)",
    )
    ax.scatter(hours, observed, color=POPULATION_COLOR, s=20, zorder=4)
    ax.legend(loc="upper left", fontsize=10.5)
    label_gpa_axes(ax)
    save_teaching_figure(fig, "reg-ols", "decomposition-all-observations.png")


def draw_fit_comparison(x: np.ndarray, deviations: np.ndarray) -> None:
    """全変動が同じ二つの標本の当てはまりと残差を, 同じ座標で比較する."""
    hours = 10 + 4 * x
    centered = np.array(
        [
            correlation * x + np.sqrt(1 - correlation**2) * deviations
            for correlation in (0.95, 0.45)
        ]
    )
    # 両標本を同じ倍率で縮め, 全変動をそろえたまますべての GPA を 0〜4 に収める.
    samples = 2.0 + (1.8 / np.abs(centered).max()) * centered
    fits = []
    for observed in samples:
        fitted = np.polyval(np.polyfit(hours, observed, 1), hours)
        residual_sum = np.sum((observed - fitted) ** 2)
        total_sum = np.sum((observed - observed.mean()) ** 2)
        fits.append((fitted, 1 - residual_sum / total_sum))

    for show_residuals, filename in (
        (False, "r-squared-fits.png"),
        (True, "r-squared-residuals.png"),
    ):
        fig, axes = plt.subplots(
            1, 2, figsize=(8.8, 4.2), sharex=True, sharey=True, layout="constrained"
        )
        for ax, observed, (fitted, r_squared) in zip(axes, samples, fits, strict=True):
            ax.plot(hours, fitted, color=POPULATION_COLOR)
            if show_residuals:
                ax.vlines(hours, fitted, observed, color=MISSED_COLOR, linewidth=1.5)
            ax.scatter(hours, observed, color=COVERED_COLOR, s=23, zorder=3)
            ax.set_title(rf"$R^2={r_squared:.2f}$", fontsize=17)
            label_gpa_axes(ax)
            ax.set_xticks([6, 10, 14])
        axes[1].set_ylabel("")
        save_teaching_figure(fig, "reg-ols", filename)


def draw_teaching_figures() -> None:
    """導入と最小二乗法の図を固定した小さな合成データから描く."""
    x = np.linspace(-1.4, 1.4, 24)
    noise = np.random.default_rng(42).normal(size=x.size)
    # 線形傾向を除いてから x と同じ分散にそろえる.
    deviations = noise - np.polyval(np.polyfit(x, noise, 1), x)
    deviations *= x.std() / deviations.std()
    hours = np.array([3, 5, 7, 9, 11, 13, 15, 17])
    observed = np.array([1.1, 0.7, 1.5, 1.2, 2.6, 2.1, 2.4, 3.6])

    draw_correlation_and_slopes(x, deviations)
    draw_regression_coefficients()
    draw_plane_and_curve()
    draw_population_errors()
    draw_least_squares_deviations(hours, observed)
    draw_variation_decomposition(hours, observed)
    draw_fit_comparison(x, deviations)


def main() -> None:
    plt.switch_backend("Agg")
    draw_teaching_figures()
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
