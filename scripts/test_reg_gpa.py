"""GPA の生成器を配布 CSV と照合し, 新しい標本と介入の結果を検証する."""

from pathlib import Path

import numpy as np
import pandas as pd
from make_reg_gpa import (
    STUDY_MEAN,
    STUDY_NOISE_SD,
    STUDY_PER_WORK_HOUR,
    WORK_MEAN,
    WORK_VARIANCE,
    draw_sample,
    intervene,
    legacy_sample,
)

LEGACY_CSV = (
    Path(__file__).resolve().parents[1] / "slds_data/ch11/multiple_regression.csv"
)
FLOAT_COLUMNS = ["GPA", "Study_Hours", "Sports_hours", "Part_time_Work"]
TIME_COLUMNS = ["Study_Hours", "Part_time_Work"]
LEGACY_FLOAT_ATOL = 1e-14
LARGE_SAMPLE_SIZE = 200_000


def test_legacy_sample() -> None:
    expected = pd.read_csv(
        LEGACY_CSV,
        index_col=0,
        encoding="utf-8-sig",
        converters=dict.fromkeys(FLOAT_COLUMNS, float),
    )
    actual = legacy_sample()
    pd.testing.assert_index_equal(actual.columns, expected.columns)
    pd.testing.assert_index_equal(actual.index, expected.index)
    pd.testing.assert_series_equal(actual["Scholarship"], expected["Scholarship"])

    actual_values = actual[FLOAT_COLUMNS].to_numpy()
    expected_values = expected[FLOAT_COLUMNS].to_numpy()
    max_difference = np.max(np.abs(actual_values - expected_values))
    exact = np.array_equal(actual_values, expected_values)
    print(f"Legacy CSV: exact={exact}, max abs difference={max_difference:.17g}")
    # 最下位桁の丸め差を許容し, すべての数値セルを絶対誤差で照合する.
    np.testing.assert_allclose(
        actual_values,
        expected_values,
        rtol=0,
        atol=LEGACY_FLOAT_ATOL,
        equal_nan=False,
    )

    expected_counts = [int(expected["GPA"].eq(endpoint).sum()) for endpoint in (0, 4)]
    actual_counts = [int(actual["GPA"].eq(endpoint).sum()) for endpoint in (0, 4)]
    np.testing.assert_array_equal(expected_counts, (7, 4))
    np.testing.assert_array_equal(actual_counts, expected_counts)
    print(f"GPA endpoints: 0={expected_counts[0]}, 4={expected_counts[1]}")


def test_draw_sample() -> None:
    covariances = {}
    for structure in ("mediator", "confounder"):
        first = draw_sample(np.random.default_rng(42), 100, structure)
        repeat = draw_sample(np.random.default_rng(42), 100, structure)
        pd.testing.assert_frame_equal(first, repeat, check_exact=True)

        sample = draw_sample(np.random.default_rng(42), LARGE_SAMPLE_SIZE, structure)
        covariances[structure] = sample[TIME_COLUMNS].cov().to_numpy()

    mediator_cov = covariances["mediator"]
    confounder_cov = covariances["confounder"]
    np.testing.assert_allclose(confounder_cov, mediator_cov, rtol=0.02, atol=0)
    relative_difference = np.max(np.abs((confounder_cov - mediator_cov) / mediator_cov))
    print(f"Covariances: max relative difference={relative_difference:.4%}")


def test_intervene() -> None:
    for structure, expected in (("mediator", 0.31), ("confounder", 0.25)):
        latent_difference, gpa_difference = intervene(
            np.random.default_rng(42),
            LARGE_SAMPLE_SIZE,
            structure,
            study_shift=1,
        )
        np.testing.assert_allclose(latent_difference, expected, atol=0.005, rtol=0)
        np.testing.assert_array_less(
            [0, gpa_difference], [gpa_difference, latent_difference]
        )
        print(
            f"{structure}: mean change latent={latent_difference:.9f}, GPA={gpa_difference:.9f}"
        )


def main() -> None:
    test_legacy_sample()
    print(
        f"Confounder: work mean={WORK_MEAN:.9f}, work SD={np.sqrt(WORK_VARIANCE):.9f}, "
        f"study centered intercept={STUDY_MEAN:.9f}, "
        f"study slope={STUDY_PER_WORK_HOUR:.9f}, study noise SD={STUDY_NOISE_SD:.9f}",
    )
    test_draw_sample()
    test_intervene()
    print("All GPA checks passed.")


if __name__ == "__main__":
    main()
