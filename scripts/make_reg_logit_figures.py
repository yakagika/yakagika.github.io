"""単位取得データのロジスティック回帰から, 教材の取得確率の曲線を描く.

入力は slds_data/dsp5/credit.csv.
出力は images/fields/regression/reg-logit/credit-probability.png.
"""

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import statsmodels.api as sm

CREDIT_COLUMNS = {"Study_Hours": float, "Part_time_Work": float, "Credit": int}


def load_credit(path: Path) -> pd.DataFrame:
    """週あたりの勉強時間・アルバイト時間と, 0/1 の単位取得を読み込む."""
    credit = pd.read_csv(path, usecols=list(CREDIT_COLUMNS), dtype=CREDIT_COLUMNS)
    if not np.isfinite(credit.to_numpy()).all():
        raise ValueError("単位取得データのすべての値が有限である必要があります.")
    if set(credit["Credit"]) != {0, 1}:
        raise ValueError("Credit には 0 と 1 の両方が必要です.")
    return credit


def estimate_curve(credit: pd.DataFrame) -> tuple[np.ndarray, pd.Series]:
    """勉強時間 200 点 (週あたりの時間) と, 平均アルバイト時間での取得確率を返す.

    credit の列は Study_Hours, Part_time_Work, Credit.
    """
    hours = credit[["Study_Hours", "Part_time_Work"]]
    predictors = sm.add_constant(hours)
    result = sm.Logit(credit["Credit"], predictors).fit(disp=False)

    study_hours = np.linspace(
        credit["Study_Hours"].min(),
        credit["Study_Hours"].max(),
        200,
    )
    prediction_grid = pd.DataFrame(
        {
            "const": 1.0,
            "Study_Hours": study_hours,
            "Part_time_Work": credit["Part_time_Work"].mean(),
        },
    )
    return study_hours, result.predict(prediction_grid)


def main() -> None:
    """単位取得の配布データを使って, 教材の予測確率の図を保存する."""
    root = Path(__file__).resolve().parents[1]
    credit = load_credit(root / "slds_data" / "dsp5" / "credit.csv")
    study_hours, probabilities = estimate_curve(credit)

    fig, ax = plt.subplots(figsize=(8, 5), layout="constrained")
    ax.plot(study_hours, probabilities, color="#146b8c", linewidth=2.5)
    ax.axhline(0.5, color="#626262", linestyle="--", linewidth=1)
    ax.set(
        xlabel="Study hours per week",
        ylabel="Estimated probability of earning credit",
        xlim=(study_hours[0], study_hours[-1]),
        ylim=(0, 1),
    )
    output_dir = root / "images" / "fields" / "regression" / "reg-logit"
    output_dir.mkdir(parents=True, exist_ok=True)
    fig.savefig(output_dir / "credit-probability.png", dpi=180)
    plt.close(fig)


if __name__ == "__main__":
    main()
