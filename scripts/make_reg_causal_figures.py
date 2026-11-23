"""合成 GPA の回帰係数と介入の平均差を出力する."""

import numpy as np
import pandas as pd
import statsmodels.api as sm

from make_reg_gpa import draw_sample, intervene

SAMPLE_SIZE = 200_000
SEED = 2026
STUDY_SHIFT = 1
STUDY = "Study_Hours"
WORK = "Part_time_Work"
SCHOLARSHIP = "Scholarship"


def study_slope(sample: pd.DataFrame, predictors: list[str]) -> float:
    """観測 GPA を目的変数とする切片つき最小二乗法で勉強時間の係数を求める."""
    design = sm.add_constant(sample[predictors].astype(float))
    return float(sm.OLS(sample["GPA"], design).fit().params[STUDY])


def print_comparisons() -> None:
    """各構造について同じ学生の介入前後を比べ, 本文の表を出力する."""
    print(f"n = {SAMPLE_SIZE:,} per structure, seed = {SEED}, shift = {STUDY_SHIFT} h/week")
    print(
        "| アルバイト時間の役割 | 入れた式の係数 | 入れない式の係数 "
        "| 介入の平均差 (潜在得点) | 介入の平均差 (観測 GPA) |"
    )
    print("|---|---:|---:|---:|---:|")
    for structure, label in (("mediator", "中間の変数"), ("confounder", "交絡因子")):
        sample = draw_sample(np.random.default_rng(SEED), SAMPLE_SIZE, structure)
        with_work = study_slope(sample, [SCHOLARSHIP, STUDY, WORK])
        without_work = study_slope(sample, [SCHOLARSHIP, STUDY])
        # 同じシードで, 回帰に用いた架空の学生の介入前後の差を求める.
        latent, observed = intervene(
            np.random.default_rng(SEED), SAMPLE_SIZE, structure, STUDY_SHIFT
        )
        print(
            f"| {label} | {with_work:.3f} | {without_work:.3f} "
            f"| {latent:.3f} | {observed:.3f} |"
        )


if __name__ == "__main__":
    print_comparisons()
