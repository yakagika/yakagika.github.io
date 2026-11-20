"""Regenerate the OECD simple regression figures used in two lectures.

Read fertility_rates.csv and time_used.csv from slds_data/ch11, then write
five PNGs under images/slds/ch11 and images/dsp/ch5.
"""

from pathlib import Path

import matplotlib
import numpy as np
import pandas as pd
import seaborn as sns
import statsmodels.api as sm
from adjustText import adjust_text
from matplotlib import pyplot as plt
from matplotlib.figure import Figure

ROOT = Path(__file__).resolve().parents[1]


def prepare_data(fertility: pd.DataFrame, time_use: pd.DataFrame) -> pd.DataFrame:
    """Return one row per matched country, excluding Mexico.

    The index is Country. Fertility is children per woman, and r is unpaid
    and care work as a percentage of total work time.
    """
    unpaid_and_care = time_use[["Unpaid", "Care"]].sum(axis=1)
    total_work = time_use[["Paid", "Unpaid", "Care"]].sum(axis=1)
    time_use = time_use.assign(r=unpaid_and_care / total_work)
    fertility = fertility.assign(
        Country=fertility["Country"].replace({"Türkiye": "Turkey"})
    )
    joined = fertility.merge(
        time_use[["Country", "r"]],
        on="Country",
        how="inner",
        sort=True,
        validate="one_to_one",
    )
    countries = joined.set_index("Country").dropna().drop("Mexico")
    countries["r"] = countries["r"] * 100
    return countries


def plot_countries(
    countries: pd.DataFrame, coefficients: pd.Series | None = None
) -> Figure:
    """Plot fertility against work share by country, with an optional fitted line.

    countries is indexed by Country and has r (%) and Fertility (children
    per woman) columns. coefficients contains the OLS terms const and r.
    """
    fig, ax = plt.subplots(figsize=(10, 6))
    if coefficients is None:
        points = ax.scatter(countries["r"], countries["Fertility"])
        title = "Fertility vs Unpaid and Care Work with Country Labels"
    else:
        points = ax.scatter(
            countries["r"], countries["Fertility"], color="blue", label="Data Points"
        )
        x_vals = np.linspace(countries["r"].min(), countries["r"].max(), 100)
        y_vals = coefficients["const"] + coefficients["r"] * x_vals
        ax.plot(x_vals, y_vals, color="red", linestyle="--", label="Regression Line")
        ax.legend()
        title = "Fertility vs Unpaid and Care Work with Regression Line"

    ax.set(
        xlim=(0, 50),
        ylim=(0, 2),
        xlabel="Unpaid and Care Work (%)",
        ylabel="Fertility (children per woman)",
        title=title,
    )
    ax.grid()
    texts = [
        ax.text(row.r, row.Fertility, country, fontsize=9)
        for country, row in countries.iterrows()
    ]
    adjust_text(
        texts,
        objects=points,
        ax=ax,
        expand=(1.15, 1.3),
        iter_lim=300,
        arrowprops={"arrowstyle": "-", "color": "gray", "lw": 0.5},
    )
    return fig


def plot_actual_predicted(actual: pd.Series, predicted: pd.Series) -> Figure:
    """Plot paired actual and fitted fertility (children per woman)."""
    fig, ax = plt.subplots(figsize=(8, 6))
    ax.scatter(predicted, actual, alpha=0.7, edgecolors="k")
    ax.set(xlabel="Predicted", ylabel="Actual", title="Actual vs. Predicted")
    ax.plot(
        [actual.min(), actual.max()],
        [actual.min(), actual.max()],
        color="red",
        linestyle="--",
    )
    ax.set_aspect("equal", adjustable="box")
    ax.grid()
    return fig


def plot_density(actual: pd.Series, predicted: pd.Series) -> Figure:
    """Compare the densities of observed and fitted fertility (children per woman)."""
    fig, ax = plt.subplots(figsize=(16, 8))
    sns.kdeplot(predicted, label="Predicted", ax=ax)
    sns.kdeplot(actual, label="Actual", ax=ax)
    ax.set(
        title="Actual/Predicted",
        xlabel="Fertility (children per woman)",
        ylabel="Density",
    )
    ax.legend()
    return fig


def main() -> None:
    matplotlib.use("Agg")
    data_dir = ROOT / "slds_data/ch11"
    fertility = pd.read_csv(data_dir / "fertility_rates.csv")
    time_use = pd.read_csv(data_dir / "time_used.csv")
    countries = prepare_data(fertility, time_use)
    actual = countries["Fertility"]
    predictors = sm.add_constant(countries[["r"]])
    result = sm.OLS(actual, predictors).fit()
    predicted = result.predict(predictors)

    slds_images = ROOT / "images/slds/ch11"
    dsp_images = ROOT / "images/dsp/ch5"
    figures = [
        (plot_countries(countries), [slds_images / "oecd_scatter_no_line.png"]),
        (
            plot_countries(countries, result.params),
            [slds_images / "oecd_scatter_with_line.png"],
        ),
        (
            plot_actual_predicted(actual, predicted),
            [dsp_images / "oecd-pred-actual.png", slds_images / "oecd_pred_actual.png"],
        ),
        (plot_density(actual, predicted), [slds_images / "oecd_density.png"]),
    ]
    for fig, paths in figures:
        for path in paths:
            fig.savefig(path, dpi=100)
        plt.close(fig)


if __name__ == "__main__":
    main()
