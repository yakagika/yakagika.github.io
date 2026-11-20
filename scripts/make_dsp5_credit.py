"""dsp 第5章のロジスティック回帰の例題データ (架空の大学生の単位取得) を作る.

出力: slds_data/dsp5/credit.csv (200 行. 列 Study_Hours, Part_time_Work, Credit).
Credit は対数オッズが勉強時間とアルバイト時間の 1 次式になるように生成する.
"""

from pathlib import Path

import numpy as np
import pandas as pd

N_STUDENTS = 200
SEED = 5
INTERCEPT = -2.0
COEF_STUDY = 0.5
COEF_WORK = -0.15


def make_credit_data(rng: np.random.Generator) -> pd.DataFrame:
    """勉強時間, アルバイト時間 (いずれも 1 週間の時間) と単位取得 (1/0) を生成する."""
    study = np.clip(rng.normal(8, 4, N_STUDENTS), 0, 25).round(1)
    work = np.clip(rng.normal(12, 6, N_STUDENTS), 0, 30).round(1)
    logit = INTERCEPT + COEF_STUDY * study + COEF_WORK * work
    prob = 1 / (1 + np.exp(-logit))
    credit = rng.binomial(1, prob)
    return pd.DataFrame(
        {"Study_Hours": study, "Part_time_Work": work, "Credit": credit},
    )


def main() -> None:
    out = Path(__file__).resolve().parent.parent / "slds_data" / "dsp5" / "credit.csv"
    df = make_credit_data(np.random.default_rng(SEED))
    df.to_csv(out, index=False)
    print(out, len(df), df["Credit"].mean())


if __name__ == "__main__":
    main()
