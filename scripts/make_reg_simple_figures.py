"""Create OECD regression figures from the two teaching CSVs in slds_data/ch11.

Writes three PNGs to images/fields/regression/reg-simple/ for the 21-country fit.
"""

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import statsmodels.api as sm
from adjustText import adjust_text
from matplotlib.axes import Axes
from matplotlib.collections import PathCollection
from matplotlib.figure import Figure
from matplotlib.text import Text


def plot_countries(data: pd.DataFrame) -> tuple[Figure, Axes, PathCollection, list[Text]]:
    """Plot r (percentage points) and Fertility (children per woman), by country."""
    fig, ax = plt.subplots(figsize=(10, 6), layout="constrained")
    points = ax.scatter(data["r"], data["Fertility"], color="#23658b", zorder=3)
    ax.set(
        xlim=(8, 49),
        ylim=(0.82, 1.98),
        xlabel="Men's unpaid work and care (% of work time)",
        ylabel="Total fertility rate (children per woman)",
    )
    ax.grid(alpha=0.25)
    labels = [
        ax.text(row.r, row.Fertility, country, fontsize=9)
        for country, row in data.iterrows()
    ]
    return fig, ax, points, labels


def main() -> None:
    root = Path(__file__).resolve().parents[1]
    output = root / "images/fields/regression/reg-simple"
    output.mkdir(parents=True, exist_ok=True)

    fertility = pd.read_csv(root / "slds_data/ch11/fertility_rates.csv")
    time_use = pd.read_csv(root / "slds_data/ch11/time_used.csv")

    unpaid_and_care = time_use["Unpaid"] + time_use["Care"]
    total_work = time_use["Paid"] + unpaid_and_care
    time_use["r"] = 100 * unpaid_and_care / total_work

    fertility["Country"] = fertility["Country"].replace({"Türkiye": "Turkey"})
    joined = fertility.merge(
        time_use[["Country", "r"]],
        on="Country",
        how="outer",
        validate="one_to_one",
        indicator=True,
    )

    matched = joined.loc[joined["_merge"] == "both", ["Country", "Fertility", "r"]]
    countries = matched.set_index("Country").dropna()
    analysis = countries.drop(index="Mexico")

    predictors = sm.add_constant(analysis[["r"]])
    result = sm.OLS(analysis["Fertility"], predictors).fit()

    fig, ax, points, labels = plot_countries(analysis)
    adjust_text(
        labels,
        objects=points,
        ax=ax,
        iter_lim=300,
        min_arrow_len=4,
        arrowprops={"arrowstyle": "-", "color": "0.45", "linewidth": 0.6},
    )
    fig.savefig(output / "scatter.png", dpi=150)
    plt.close(fig)

    work_share = np.linspace(analysis["r"].min(), analysis["r"].max(), 100)
    fitted_line = result.params["const"] + result.params["r"] * work_share

    fig, ax, points, labels = plot_countries(analysis)
    ax.plot(work_share, fitted_line, color="#b44532", label="OLS line")
    ax.legend(loc="lower right")
    adjust_text(
        labels,
        objects=points,
        ax=ax,
        iter_lim=300,
        min_arrow_len=4,
        arrowprops={"arrowstyle": "-", "color": "0.45", "linewidth": 0.6},
    )
    fig.savefig(output / "regression-line.png", dpi=150)
    plt.close(fig)

    observed = analysis["Fertility"]
    fitted = result.fittedvalues
    limits = (0.85, 1.95)

    fig, ax = plt.subplots(figsize=(6, 6), layout="constrained")
    ax.scatter(fitted, observed, color="#23658b", alpha=0.8)
    ax.plot(limits, limits, color="#b44532", linestyle="--", label="Exact fit")
    ax.set(
        xlim=limits,
        ylim=limits,
        xlabel="Fitted total fertility rate",
        ylabel="Observed total fertility rate",
        aspect="equal",
    )
    ax.legend()
    ax.grid(alpha=0.25)
    fig.savefig(output / "fitted-observed.png", dpi=150)
    plt.close(fig)


if __name__ == "__main__":
    main()
