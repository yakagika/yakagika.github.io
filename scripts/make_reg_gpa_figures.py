"""Create GPA regression figures from the teaching CSV in slds_data/ch11.

Writes seven PNGs to images/fields/regression/reg-gpa/ using the 70 training rows.
"""

from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
import statsmodels.formula.api as smf
from sklearn.model_selection import train_test_split
from statsmodels.graphics.regressionplots import plot_partregress_grid


def plot_training_data(train: pd.DataFrame, output: Path) -> None:
    """Save distributions and correlations for the 70 training students."""
    numeric_columns = ["GPA", "Study_Hours", "Sports_hours", "Part_time_Work"]
    fig, axes = plt.subplots(4, 4, figsize=(10, 9), layout="constrained")
    pd.plotting.scatter_matrix(
        train[numeric_columns],
        ax=axes,
        diagonal="hist",
        range_padding=0.12,
        color="#23658b",
        alpha=0.7,
    )
    fig.savefig(output / "scatter-matrix.png", dpi=150)
    plt.close(fig)

    correlations = train[numeric_columns].corr()
    fig, ax = plt.subplots(figsize=(8, 6), layout="constrained")
    sns.heatmap(
        correlations,
        ax=ax,
        vmin=-1,
        vmax=1,
        center=0,
        cmap="RdBu_r",
        annot=True,
        fmt=".2f",
        square=True,
    )
    ax.tick_params(axis="x", labelrotation=25)
    ax.tick_params(axis="y", labelrotation=0)
    fig.savefig(output / "correlations.png", dpi=150)
    plt.close(fig)

    groups = sns.pairplot(
        train[[*numeric_columns, "Scholarship"]],
        hue="Scholarship",
        diag_kind="hist",
        palette="Set2",
        diag_kws={"bins": 8},
        plot_kws={"alpha": 0.7},
        height=2.2,
    )
    groups.figure.savefig(output / "scholarship-pairs.png", dpi=150)
    plt.close(groups.figure)

    joint = sns.JointGrid(
        data=train,
        x="Study_Hours",
        y="GPA",
        hue="Scholarship",
        palette="Set2",
        height=6,
    )
    joint.plot_joint(sns.scatterplot, alpha=0.75)
    joint.plot_marginals(sns.histplot, bins=8)
    joint.figure.savefig(output / "scholarship-study.png", dpi=150)
    plt.close(joint.figure)


def plot_model_fit(train: pd.DataFrame, output: Path) -> None:
    """Fit GPA on scholarship and weekly hours, then plot the in-sample fit."""
    formula = "GPA ~ Scholarship + Study_Hours + Part_time_Work"
    result = smf.ols(formula, data=train).fit()

    observed = train["GPA"]
    fitted = result.fittedvalues
    limits = (
        min(0, fitted.min()) - 0.1,
        max(4, fitted.max()) + 0.1,
    )

    fig, ax = plt.subplots(figsize=(6, 6), layout="constrained")
    ax.scatter(fitted, observed, color="#23658b", alpha=0.75)
    ax.plot(limits, limits, color="#b44532", linestyle="--", label="Exact fit")
    ax.set(
        xlabel="Fitted GPA",
        ylabel="Observed GPA",
        xlim=limits,
        ylim=limits,
        aspect="equal",
    )
    ax.legend()
    ax.grid(alpha=0.25)
    fig.savefig(output / "fitted-observed.png", dpi=150)
    plt.close(fig)

    fig, ax = plt.subplots(figsize=(8, 4), layout="constrained")
    sns.kdeplot(observed, ax=ax, clip=(0, 4), cut=0, label="Observed", color="#23658b")
    sns.kdeplot(fitted, ax=ax, cut=0, label="Fitted", color="#b44532")
    ax.set(xlabel="GPA", ylabel="Density")
    ax.legend()
    fig.savefig(output / "fitted-density.png", dpi=150)
    plt.close(fig)

    fig = plt.figure(figsize=(11, 4))
    plot_partregress_grid(
        result,
        exog_idx=["Scholarship[T.True]", "Study_Hours", "Part_time_Work"],
        grid=(1, 3),
        fig=fig,
    )
    fig.subplots_adjust(top=0.84)
    fig.savefig(output / "partial-regression.png", dpi=150)
    plt.close(fig)


def main() -> None:
    root = Path(__file__).resolve().parents[1]
    output = root / "images/fields/regression/reg-gpa"
    output.mkdir(parents=True, exist_ok=True)

    df = pd.read_csv(root / "slds_data/ch11/multiple_regression.csv", index_col=0)
    train, _ = train_test_split(df, test_size=0.3, random_state=0)

    plot_training_data(train, output)
    plot_model_fit(train, output)


if __name__ == "__main__":
    main()
