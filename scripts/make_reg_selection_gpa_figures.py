"""Plot distributions and correlations for the GPA exploration sample.

Reads the teaching CSV in slds_data/ch11 and saves two PNGs under
images/fields/regression/reg-selection-gpa.
"""

from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
from sklearn.model_selection import train_test_split


def plot_exploration_data(explore: pd.DataFrame, output: Path) -> None:
    """Save a scatter matrix and Pearson correlations for the 35 exploration rows."""
    numeric_columns = ["GPA", "Study_Hours", "Sports_hours", "Part_time_Work"]
    fig, axes = plt.subplots(4, 4, figsize=(10, 9), layout="constrained")
    pd.plotting.scatter_matrix(
        explore[numeric_columns],
        ax=axes,
        diagonal="hist",
        range_padding=0.12,
        color="#23658b",
        alpha=0.7,
    )
    fig.savefig(output / "scatter-matrix.png", dpi=150)
    plt.close(fig)

    correlations = explore[numeric_columns].corr()
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


def main() -> None:
    root = Path(__file__).resolve().parents[1]
    df = pd.read_csv(root / "slds_data/ch11/multiple_regression.csv", index_col=0)
    train, _ = train_test_split(df, test_size=0.3, random_state=0)
    explore, _ = train_test_split(train, test_size=0.5, random_state=0)

    output = root / "images/fields/regression/reg-selection-gpa"
    output.mkdir(parents=True, exist_ok=True)
    plot_exploration_data(explore, output)


if __name__ == "__main__":
    main()
