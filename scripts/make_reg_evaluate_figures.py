"""Plot prediction intervals for the held-out students in the teaching GPA CSV.

Reads slds_data/ch11/multiple_regression.csv, prints the counts shown in the lesson,
and writes test-prediction-intervals.png under images/fields/regression/reg-evaluate.
"""

from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd
import statsmodels.formula.api as smf
from sklearn.model_selection import train_test_split


def plot_intervals(intervals: pd.DataFrame, observed: pd.Series, output: Path) -> None:
    """Save one interval per held-out student, aligned to observed by row index."""
    ordered = intervals.sort_values("mean")
    observed = observed.loc[ordered.index]
    ranks = range(1, len(ordered) + 1)

    fig, ax = plt.subplots(figsize=(8, 4.5), layout="constrained")
    ax.axhline(0, color="0.45", linestyle="--", linewidth=1)
    ax.axhline(4, color="0.45", linestyle="--", linewidth=1, label="GPA bounds")
    ax.vlines(
        ranks,
        ordered["obs_ci_lower"],
        ordered["obs_ci_upper"],
        color="#91b5ca",
        linewidth=2,
        label="95% prediction interval",
        zorder=1,
    )
    ax.scatter(
        ranks,
        ordered["mean"],
        marker="x",
        color="#23658b",
        label="Predicted GPA",
        zorder=3,
    )
    ax.scatter(ranks, observed, s=24, color="#b44532", label="Observed GPA", zorder=3)
    ax.set(
        xlabel="Test student rank (by predicted GPA)",
        ylabel="GPA (points)",
        xticks=[1, 5, 10, 15, 20, 25, 30],
    )
    ax.legend(loc="upper left")
    fig.savefig(output / "test-prediction-intervals.png", dpi=150)
    plt.close(fig)


def main() -> None:
    root = Path(__file__).resolve().parents[1]
    df = pd.read_csv(root / "slds_data/ch11/multiple_regression.csv", index_col=0)
    train, test = train_test_split(df, test_size=0.3, random_state=0)
    formula = "GPA ~ Scholarship + Study_Hours + Part_time_Work"
    result = smf.ols(formula, data=train).fit()
    intervals = result.get_prediction(test).summary_frame(alpha=0.05)
    observed = test["GPA"]

    covered = observed.between(intervals["obs_ci_lower"], intervals["obs_ci_upper"])
    print(f"Covered: {covered.sum()}/{len(test)}")
    print(f"Prediction intervals below 0: {(intervals['obs_ci_lower'] < 0).sum()}")
    print(f"Prediction intervals above 4: {(intervals['obs_ci_upper'] > 4).sum()}")
    print(f"Predicted GPA above 4: {(intervals['mean'] > 4).sum()}")

    output = root / "images/fields/regression/reg-evaluate"
    output.mkdir(parents=True, exist_ok=True)
    plot_intervals(intervals, observed, output)


if __name__ == "__main__":
    main()
