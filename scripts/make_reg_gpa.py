"""回帰分析の例題で使う架空の学生の GPA を生成する.

配布した 100 人の標本を再現し, 指定した乱数生成器から新しい標本と介入の平均差を求める.

交絡構造では, アルバイト時間の平均を 14, 分散を 9.76 (= 0.6**2 * 16 + 2**2) とする.
勉強時間を 10 + 傾き * (アルバイト時間 - 14) + 雑音とし, 傾きは共分散 -9.6 を 9.76 で割る.
雑音の分散は 16 - (-9.6)**2 / 9.76 となり, 2 つの構造で時間の同時正規分布が一致する.
"""

import numpy as np
import pandas as pd

LEGACY_SEED = 42
LEGACY_SIZE = 100
STUDY_MEAN = 10
STUDY_SD = 4
WORK_INTERCEPT = 20
WORK_PER_STUDY_HOUR = -0.6
WORK_NOISE_SD = 2
GPA_PER_STUDY_HOUR = 0.25
GPA_PER_WORK_HOUR = -0.1

WORK_MEAN = WORK_INTERCEPT + WORK_PER_STUDY_HOUR * STUDY_MEAN
WORK_VARIANCE = (WORK_PER_STUDY_HOUR * STUDY_SD) ** 2 + WORK_NOISE_SD**2
STUDY_WORK_COVARIANCE = WORK_PER_STUDY_HOUR * STUDY_SD**2
STUDY_PER_WORK_HOUR = STUDY_WORK_COVARIANCE / WORK_VARIANCE
STUDY_NOISE_SD = np.sqrt(STUDY_SD**2 - STUDY_PER_WORK_HOUR * STUDY_WORK_COVARIANCE)


def legacy_sample() -> pd.DataFrame:
    """配布版と同じ乱数列で 100 人を生成し, 配布版の 5 列を返す."""
    rng = np.random.RandomState(LEGACY_SEED)
    return _draw_sample(rng, LEGACY_SIZE, "mediator").drop(columns="GPA_latent")


def draw_sample(rng: np.random.Generator, n: int, structure: str) -> pd.DataFrame:
    """指定の構造で n 人を生成し, 配布版の 5 列と潜在得点 GPA_latent を返す.

    時間の単位は週あたりの時間とする. structure は "mediator" または "confounder" とする.
    GPA は潜在得点を 0 から 4 の範囲に収めた値とする.
    """
    return _draw_sample(rng, n, structure)


def _draw_sample(
    rng: np.random.Generator | np.random.RandomState,
    n: int,
    structure: str,
) -> pd.DataFrame:
    """週あたりの各時間と GPA を生成する. 配布版の乱数を引く順序を保つ."""
    if structure not in ("mediator", "confounder"):
        raise ValueError(f"Unknown GPA structure: {structure!r}")

    scholarship = rng.choice([True, False], size=n)
    if structure == "mediator":
        study = rng.normal(STUDY_MEAN, STUDY_SD, size=n)
        sports = rng.normal(5, 2.0, size=n)
        work = WORK_INTERCEPT + WORK_PER_STUDY_HOUR * study
        work += rng.normal(0, WORK_NOISE_SD, size=n)
    else:
        work = rng.normal(WORK_MEAN, np.sqrt(WORK_VARIANCE), size=n)
        study = STUDY_MEAN + STUDY_PER_WORK_HOUR * (work - WORK_MEAN)
        study += rng.normal(0, STUDY_NOISE_SD, size=n)
        sports = rng.normal(5, 2.0, size=n)

    gpa_latent = (
        0.5
        + 0.6 * scholarship
        + GPA_PER_STUDY_HOUR * study
        + GPA_PER_WORK_HOUR * work
        + rng.normal(0, 0.1, size=n)
    )
    return pd.DataFrame(
        {
            "GPA": np.clip(gpa_latent, 0, 4),
            "Scholarship": scholarship,
            "Study_Hours": study,
            "Sports_hours": sports,
            "Part_time_Work": work,
            "GPA_latent": gpa_latent,
        },
    )


def intervene(
    rng: np.random.Generator,
    n: int,
    structure: str,
    study_shift: float,
) -> tuple[float, float]:
    """勉強時間を週あたり study_shift 時間ずらしたときの平均差を返す.

    同じ n 人について介入後から介入前を引き, 潜在得点と GPA の順に平均差を返す.
    """
    if n <= 0:
        raise ValueError("An intervention requires at least one student.")

    sample = draw_sample(rng, n, structure)
    work_shift = 0.0
    if structure == "mediator":
        work_shift = WORK_PER_STUDY_HOUR * study_shift

    # 構造式の差を使うと, 個人ごとの雑音を固定したまま介入後の得点を求められる.
    latent_shift = GPA_PER_STUDY_HOUR * study_shift + GPA_PER_WORK_HOUR * work_shift
    intervened_latent = sample["GPA_latent"] + latent_shift
    intervened_gpa = np.clip(intervened_latent, 0, 4)
    return (
        float((intervened_latent - sample["GPA_latent"]).mean()),
        float((intervened_gpa - sample["GPA"]).mean()),
    )
