"""Generate two regression figures from the synthetic GPA population.

The input is the mediator generator in make_reg_gpa. The output is PNGs under
images/fields/regression/reg-prediction and the values used in the lesson.
"""

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import statsmodels.formula.api as smf
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error

from make_reg_gpa import draw_sample

BLUE = "#23658b"
ORANGE = "#b44532"


def interval_widths() -> pd.DataFrame:
    """Return mean full widths in latent GPA points over 200 samples per size."""
    rng = np.random.default_rng(20261120)
    target = pd.DataFrame({"Study_Hours": [10.0]})
    records = []
    for sample_size in (20, 100, 1000):
        mean_widths = []
        prediction_widths = []
        for _ in range(200):
            sample = draw_sample(rng, sample_size, "mediator")
            result = smf.ols("GPA_latent ~ Study_Hours", data=sample).fit()
            intervals = result.get_prediction(target).summary_frame(alpha=0.05).iloc[0]
            mean_widths.append(intervals["mean_ci_upper"] - intervals["mean_ci_lower"])
            prediction_widths.append(
                intervals["obs_ci_upper"] - intervals["obs_ci_lower"]
            )
        records.append((sample_size, np.mean(mean_widths), np.mean(prediction_widths)))
    return pd.DataFrame(records, columns=["sample_size", "mean_ci_width", "pi_width"])


def overfit_errors() -> pd.DataFrame:
    """Return MSE averaged over 200 independent train and evaluation samples."""
    rng = np.random.default_rng(20261121)
    repetitions = 200
    # Leave at least six residual degrees of freedom after the intercept and study time.
    max_noise_columns = 12
    noise_counts = range(max_noise_columns + 1)
    training_mse_sum = np.zeros(len(noise_counts))
    new_mse_sum = np.zeros(len(noise_counts))

    for _ in range(repetitions):
        train = draw_sample(rng, 20, "mediator")
        new_students = draw_sample(rng, 20_000, "mediator")
        train_predictors = np.column_stack(
            (train["Study_Hours"], rng.standard_normal((len(train), max_noise_columns)))
        )
        new_predictors = np.column_stack(
            (
                new_students["Study_Hours"],
                rng.standard_normal((len(new_students), max_noise_columns)),
            )
        )
        for noise_columns in noise_counts:
            columns = slice(0, noise_columns + 1)
            train_subset = train_predictors[:, columns]
            new_subset = new_predictors[:, columns]
            model = LinearRegression().fit(train_subset, train["GPA_latent"])
            training_mse_sum[noise_columns] += mean_squared_error(
                train["GPA_latent"], model.predict(train_subset)
            )
            new_mse_sum[noise_columns] += mean_squared_error(
                new_students["GPA_latent"], model.predict(new_subset)
            )

    return pd.DataFrame(
        {
            "noise_variables": noise_counts,
            "training_mse": training_mse_sum / repetitions,
            "new_mse": new_mse_sum / repetitions,
        }
    )


def plot_interval_widths(widths: pd.DataFrame, output: Path) -> None:
    """Plot full interval widths (latent GPA points) against sample size."""
    fig, ax = plt.subplots(figsize=(7, 4), layout="constrained")
    ax.plot(
        widths["sample_size"],
        widths["mean_ci_width"],
        color=BLUE,
        marker="o",
        label="95% confidence interval for the mean",
    )
    ax.plot(
        widths["sample_size"],
        widths["pi_width"],
        color=ORANGE,
        marker="s",
        label="95% prediction interval",
    )
    ax.set(
        xlabel="Training sample size (students, log scale)",
        ylabel="Average interval width (latent GPA points)",
        xscale="log",
        ylim=(0, 2.15),
    )
    ax.set_xticks(widths["sample_size"], labels=["20", "100", "1,000"])
    ax.legend()
    ax.grid(axis="y", alpha=0.25)
    fig.savefig(output / "interval-widths.png", dpi=150)
    plt.close(fig)


def plot_overfit_errors(errors: pd.DataFrame, output: Path) -> None:
    """Plot average MSE for 20 training and 20,000 new students per repetition."""
    fig, ax = plt.subplots(figsize=(7, 4), layout="constrained")
    ax.plot(
        errors["noise_variables"],
        errors["training_mse"],
        color=BLUE,
        marker="o",
        label="Training students (20 per repetition)",
    )
    ax.plot(
        errors["noise_variables"],
        errors["new_mse"],
        color=ORANGE,
        marker="s",
        label="New students (20,000 per repetition)",
    )
    ax.set(
        xlabel="Added noise variables (count)",
        ylabel="Average MSE (latent GPA points squared)",
        xticks=errors["noise_variables"].iloc[::2],
        ylim=(0, None),
    )
    ax.legend()
    ax.grid(axis="y", alpha=0.25)
    fig.savefig(output / "overfitting-mse.png", dpi=150)
    plt.close(fig)


def main() -> None:
    root = Path(__file__).resolve().parents[1]
    output = root / "images/fields/regression/reg-prediction"
    output.mkdir(parents=True, exist_ok=True)

    widths = interval_widths()
    errors = overfit_errors()
    print(widths.to_string(index=False, float_format=lambda value: f"{value:.3f}"))
    print(errors.to_string(index=False, float_format=lambda value: f"{value:.3f}"))
    plot_interval_widths(widths, output)
    plot_overfit_errors(errors, output)


if __name__ == "__main__":
    main()
