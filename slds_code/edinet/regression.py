"""分析用データを可視化し, 重回帰分析を行う.

実行方法:
    uv run python regression.py

入力:  data/analysis_table.csv (merge_all.py の出力)
出力:  result/ 以下に散布図行列・相関ヒートマップ・回帰結果

回帰の方針 (詳細は講義資料 Ch11 と補足A を参照):
    - 関心のある説明変数 (ここでは ESG スコア) を含む全変数モデルを基本とする.
    - 産業と年度の影響はダミー変数で統制する.
    - AIC によるモデル比較は「候補モデルをあらかじめ決めて比較する」形で行い,
      p 値を見てから変数を選び直すことはしない (多重検定の問題が生じるため).
"""

from pathlib import Path

import matplotlib_fontja  # noqa: F401  (matplotlib の日本語表示)
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
import statsmodels.api as sm

DATA_DIR = Path(__file__).parent / "data"
RESULT_DIR = Path(__file__).parent / "result"

TARGET = "ROA (総資産利益率)"          # 目的変数
FOCUS = "ESGスコア"                    # 関心のある説明変数
CONTROLS = ["自己資本比率", "売上高経常利益率", "資本生産性"]  # 統制変数
NUMERIC_COLS = [FOCUS, TARGET] + CONTROLS


def load_data() -> pd.DataFrame:
    df = pd.read_csv(DATA_DIR / "analysis_table.csv", dtype={"secCode": str})
    if "ESGスコア" not in df.columns:
        raise SystemExit("ESG スコアが結合されていません (merge_all.py の説明を参照).")
    # 使う変数が揃っている行だけに絞る (変数単位の欠損処理)
    df = df.dropna(subset=NUMERIC_COLS + ["産業大分類"]).copy()
    # サンプルが極端に少ない産業は除外する (係数が 1-2 社で決まってしまうため)
    counts = df["産業大分類"].value_counts()
    df = df[df["産業大分類"].isin(counts[counts >= 10].index)]
    return df


def visualize(df: pd.DataFrame) -> None:
    (RESULT_DIR).mkdir(parents=True, exist_ok=True)

    axes = pd.plotting.scatter_matrix(df[NUMERIC_COLS], figsize=(12, 12))
    for ax in axes.ravel():
        ax.set_xlabel(ax.get_xlabel(), rotation=30)
        ax.set_ylabel(ax.get_ylabel(), rotation=0, ha="right")
    plt.suptitle("散布図行列", y=0.98)
    plt.savefig(RESULT_DIR / "scatter_matrix.png", dpi=300, bbox_inches="tight")
    plt.close()

    plt.figure(figsize=(8, 6))
    sns.heatmap(df[NUMERIC_COLS].corr(), vmin=-1, vmax=1, annot=True, cmap="RdBu_r")
    plt.title("相関ヒートマップ")
    plt.savefig(RESULT_DIR / "corr_heatmap.png", dpi=300, bbox_inches="tight")
    plt.close()


def run_regression(df: pd.DataFrame) -> None:
    # 量的変数の標準化 (係数の大きさを比較しやすくする)
    X_num = df[[FOCUS] + CONTROLS].astype(float)
    X_num = (X_num - X_num.mean()) / X_num.std(ddof=0)

    dummies = pd.get_dummies(
        df[["産業大分類", "年度"]].astype(str), drop_first=True, dtype=float
    )
    X = sm.add_constant(pd.concat([X_num, dummies], axis=1))
    y = df[TARGET].astype(float)

    # モデル 1: 統制変数のみ / モデル 2: + ESG スコア を AIC で比較する
    base_cols = [c for c in X.columns if c != FOCUS]
    model_base = sm.OLS(y, X[base_cols]).fit()
    model_full = sm.OLS(y, X).fit()

    print(f"AIC (統制変数のみ): {model_base.aic:.1f}")
    print(f"AIC (+ {FOCUS}):    {model_full.aic:.1f}")
    print(model_full.summary())

    (RESULT_DIR / "regression_summary.txt").write_text(
        str(model_full.summary()), encoding="utf-8"
    )

    # 実測値 vs 予測値
    pred = model_full.predict(X)
    plt.figure(figsize=(8, 6))
    plt.scatter(y, pred, alpha=0.7, edgecolors="k")
    lims = [y.min(), y.max()]
    plt.plot(lims, lims, color="red", linestyle="--")
    plt.xlabel(f"実測値 ({TARGET})")
    plt.ylabel(f"予測値 ({TARGET})")
    plt.title("実測値 vs 予測値")
    plt.savefig(RESULT_DIR / "actual_vs_predicted.png", dpi=300, bbox_inches="tight")
    plt.close()


def main() -> None:
    df = load_data()
    print(f"分析対象: {len(df)} 行, 企業 {df['secCode'].nunique()} 社")
    visualize(df)
    run_regression(df)


if __name__ == "__main__":
    main()
