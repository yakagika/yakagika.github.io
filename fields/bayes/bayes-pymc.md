---
title: "PyMC によるモデルの実装"
kind: code
open: true
requires:
  - bayes-modeling
  - bayes-setup
---
## PyMC 実装 {#pymc-実装}

コードが複雑になるため, 先にコードの全体像を示した後, 個別に意味を解説します.
まずは, 以下のコードを作業ディレクトリに配置した後, 実行してみましょう.

::: warn

コードは `save_dir = 'images/'` (プロジェクト直下の `images/` ディレクトリ) を前提としています. 前節で作成した `images/` ディレクトリと, 講義配布の `hierarchical_regression.csv` がプロジェクト直下に置かれた状態で実行してください. 別の場所に保存したい場合は `save_dir` を書き換えてください.

:::

~~~ py
import pymc as pm
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import arviz as az
import graphviz
from pymc import model_to_graphviz
import statsmodels.api as sm
from sklearn.metrics import mean_squared_error, r2_score, mean_absolute_error
from scipy.stats import norm, rankdata


save_dir = 'images/'

if __name__ == "__main__":

    # ---------------------------
    # 1. データ読み込み
    # ---------------------------
    df = pd.read_csv('hierarchical_regression.csv'
                ,dtype={'GPA': float
                       ,'Scholarship': bool
                       ,'Study_Hours': float
                       ,'Sports_hours': float
                       ,'Part_time_Work': float
                       ,'StudentID':int})

    # 数量化
    df = pd.get_dummies(df, columns=['Scholarship'], dtype='int')

    #標準化
    from sklearn.preprocessing import StandardScaler
    scaler = StandardScaler()
    df[['Scholarship_True'
       ,'Study_Hours'
       ,'Part_time_Work'
       ,'Sports_hours']] = scaler.fit_transform(df[['Scholarship_True'
                                                   ,'Study_Hours'
                                                   ,'Part_time_Work'
                                                   ,'Sports_hours']])

    # ---------------------------
    # ２. 線形回帰
    # ---------------------------
    
    # 説明変数(X)と目的変数(y)に分割
    X = df[['Scholarship_True', 'Study_Hours']]
    y = df['GPA']


    # 切片(定数項)を追加
    X = sm.add_constant(X)
    lm = sm.OLS(y, X).fit()
    print("\n[通常の線形回帰の結果]\n")
    print(lm.summary())
    lm_preds = lm.predict(X)

    plt.figure(figsize=(8, 6))
    plt.scatter(df['GPA'], lm_preds, alpha=0.7, edgecolors="k")
    plt.xlabel("Predicted")
    plt.ylabel("Actual")
    plt.title("Actual vs. Predicted")
    plt.plot([df['GPA'].min(), df['GPA'].max()], [df['GPA'].min(), df['GPA'].max()], color="red", linestyle="--")  # 完全一致のライン
    plt.grid()
    plt.savefig(save_dir + 'lm.png')
    plt.close()

    plt.figure(figsize=(16,8))
    sns.kdeplot(lm_preds, label = 'Predicted')
    sns.kdeplot(df['GPA'], label = 'Actual')
    plt.title('Actual/Predicted')
    plt.xlabel('GPA')
    plt.ylabel('Density')
    plt.legend()
    plt.savefig(save_dir + 'lm_kde.png')
    plt.close()
    
    # ---------------------------
    # ３. 一般化線形モデル
    # ---------------------------

    coords = {
        "student": df["StudentID"].values,
        "obs_id": np.arange(len(df))
    }

    with pm.Model(coords=coords) as model:
        st = pm.Data("st", df["Study_Hours"])
        #ptw = pm.Data("ptw", df["Part_time_Work"])
        ss = pm.Data("ss", df["Scholarship_True"])
        student_idx = pm.Data("student_idx", df["StudentID"])

        #ランダム切片
        z_alpha = pm.Normal("z_alpha", 0, 1, dims="student")
        mu_alpha = pm.Normal("mu_alpha", 0, 1)
        sigma_alpha = pm.HalfNormal("sigma_alpha", 1)
        alpha = pm.Deterministic("alpha", mu_alpha + sigma_alpha * z_alpha, dims="student")


        # 勉強時間の効果
        beta_st = pm.Normal("beta_st",0,1)
        
        # 奨学金の効果
        beta_ss = pm.Normal("beta_ss",0,1)
 
        mu = alpha[student_idx] + beta_st * st +  beta_ss * ss

        #sigma = pm.HalfNormal("sigma", 1)
        sigma = 0.3
        gpa_obs = pm.Normal("GPA",
                            mu=mu,
                            sigma=sigma,
                            observed=df["GPA"],
                            dims="obs_id"
                        )
        trace = pm.sample(draws =2000, tune=2000, chains=4, target_accept=0.95, return_inferencedata=True)
        pm.sample_posterior_predictive(trace, extend_inferencedata=True)

    # ---------------------------
    # 4. モデル診断と可視化
    # ---------------------------
    graph = model_to_graphviz(model)
    graph.render(filename= save_dir + "hierarchical_bayes_model", format="pdf")

    # alpha は学生数分の次元を持つため,
    # trace プロットでは代表 10 名分にサブセットして可視化する
    sample_students = list(trace.posterior.coords["student"].values[:10])

    az.plot_trace(trace
                 ,var_names=["sigma_alpha"
                            ,"alpha"
                            ,"beta_st"
                            ,"beta_ss"]
                 ,coords={"student": sample_students})
    plt.tight_layout()
    plt.savefig(save_dir + 'trace.png')
    plt.close()

    print(az.summary(trace
                    ,var_names=["sigma_alpha"
                               ,"alpha"
                               ,"beta_st"
                               ,"beta_ss"]))
    summary_df = az.summary(trace
                           ,var_names=["sigma_alpha"
                                      ,"alpha"
                                      ,"beta_st"
                                      ,"beta_ss"])
    summary_df.to_csv("bayesian_summary.csv"
                     ,encoding="utf-8-sig")


    # ---------------------------
    # 6. 階層ベイズモデルによる予測精度評価
    # ---------------------------
    idata = trace

    # 事後予測チェック (PPC) を手動で描画
    # arviz>=1.0 では plot_ppc が arviz-plots に切り出され,
    # 標準の `import arviz as az` から `az.plot_ppc` が消えるバージョンがあるため,
    # ここでは matplotlib/seaborn で同等のプロットを構築する.
    ppc_samples = idata.posterior_predictive["GPA"].stack(sample=("chain", "draw"))
    n_show = min(100, ppc_samples.sizes["sample"])
    rng = np.random.default_rng(0)
    sample_idx = rng.choice(ppc_samples.sizes["sample"], size=n_show, replace=False)

    fig, ax = plt.subplots(figsize=(10, 6))
    for i in sample_idx:
        sns.kdeplot(ppc_samples.isel(sample=int(i)).values,
                    color="C0", alpha=0.1, lw=1, ax=ax)
    # 凡例用のダミー線
    ax.plot([], [], color="C0", alpha=0.6, label="Posterior predictive draws")
    ppc_mean = idata.posterior_predictive["GPA"].mean(dim=("chain", "draw")).values
    sns.kdeplot(ppc_mean, color="C1", label="Posterior predictive mean", lw=2, ax=ax)
    sns.kdeplot(df["GPA"], color="black", label="Observed", lw=2, ax=ax)
    ax.set_title("Posterior Predictive Check")
    ax.set_xlabel("GPA")
    ax.legend()
    plt.savefig(save_dir + 'ppc.png')
    plt.close()


    #------------------------------------------------------------------
    # 予測値の可視化
    #------------------------------------------------------------------

    posterior_mean = idata.posterior_predictive["GPA"].mean(dim=("chain", "draw")).values
    bayes_mse = mean_squared_error(df["GPA"], posterior_mean)
    print(f"[階層ベイズモデルのMSE] {bayes_mse:.4f}\n")
    # 事後予測の平均（予測値）を取り出す
    posterior_mean = idata.posterior_predictive["GPA"].mean(dim=("chain", "draw")).values

    # プロット
    plt.figure(figsize=(8, 6))
    plt.scatter(df['GPA'], posterior_mean, alpha=0.7, edgecolors="k")
    plt.xlabel("Predicted (Bayesian)")
    plt.ylabel("Actual GPA")
    plt.title("Bayesian Actual vs. Predicted")
    plt.plot([df['GPA'].min(), df['GPA'].max()],
             [df['GPA'].min(), df['GPA'].max()],
             color="red", linestyle="--", label="Perfect prediction")
    plt.grid()
    plt.legend()
    plt.savefig(save_dir + 'bayes.png')
    plt.close()

    plt.figure(figsize=(16,8))
    sns.kdeplot(posterior_mean, label = 'Predicted')
    sns.kdeplot(df['GPA'], label = 'Actual')
    plt.title('Actual/Predicted')
    plt.xlabel('GPA')
    plt.ylabel('Density')
    plt.legend()
    plt.savefig(save_dir + 'bayes_kde.png')
    plt.close()

    #------------------------------------------------------------------
    # 予測範囲の可視化
    #------------------------------------------------------------------

    # x軸用の study_hours の範囲（100点）
    study_grid = np.linspace(df["Study_Hours"].min(), df["Study_Hours"].max(), 100)

    # 奨学金なしの予測
    predict_df_0 = pd.DataFrame({
        "Study_Hours": study_grid,
        "Scholarship_True": 0,
        "StudentID": 0
    })

    # 奨学金ありの予測
    predict_df_1 = pd.DataFrame({
        "Study_Hours": study_grid,
        "Scholarship_True": 1,
        "StudentID": 0
    })

    # より簡単な方法：事後分布から直接予測
    # 事後分布のサンプルを取得（新しいAPIを使用）
    posterior_samples = az.extract(trace)
    
    # 予測計算
    n_samples = len(posterior_samples.draw)
    n_grid = len(study_grid)
    
    # 予測値を格納する配列（奨学金なし・あり）
    predictions_0 = np.zeros((n_samples, n_grid))
    predictions_1 = np.zeros((n_samples, n_grid))
    
    # パラメータを一度に取得
    mu_alpha_vals = posterior_samples.mu_alpha.values
    sigma_alpha_vals = posterior_samples.sigma_alpha.values
    z_alpha_vals = posterior_samples.z_alpha.values  # 全学生のz_alpha    
    beta_st_vals = posterior_samples.beta_st.values    
    beta_ss_vals = posterior_samples.beta_ss.values
    
    for i in range(n_samples):
        # 各サンプルでのパラメータ（学生0の切片を計算）
        alpha_0 = mu_alpha_vals[i] + sigma_alpha_vals[i] * z_alpha_vals[0, i]  # 学生0の切片
        
        # 奨学金なしの予測値の計算
        mu_pred_0 = alpha_0 + beta_st_vals[i] * study_grid + beta_ss_vals[i] * predict_df_0["Scholarship_True"]
        predictions_0[i, :] = np.random.normal(mu_pred_0, 0.3)
        
        # 奨学金ありの予測値の計算
        mu_pred_1 = alpha_0 + beta_st_vals[i] * study_grid + beta_ss_vals[i] * predict_df_1["Scholarship_True"]
        predictions_1[i, :] = np.random.normal(mu_pred_1, 0.3)

    # 平均と予測区間（94%）
    mean_pred_0 = np.mean(predictions_0, axis=0)
    lower_pred_0 = np.percentile(predictions_0, 3, axis=0)
    upper_pred_0 = np.percentile(predictions_0, 97, axis=0)
    
    mean_pred_1 = np.mean(predictions_1, axis=0)
    lower_pred_1 = np.percentile(predictions_1, 3, axis=0)
    upper_pred_1 = np.percentile(predictions_1, 97, axis=0)

    # 実データも合わせて表示（奨学金の有無で色分け）
    plt.figure(figsize=(12, 8))
    
    # 奨学金なしの予測線
    plt.plot(study_grid, mean_pred_0, color="blue", label="Bayesian prediction (Scholarship=0)")
    plt.fill_between(study_grid, lower_pred_0, upper_pred_0, color="blue", alpha=0.2, label="94% CI (Scholarship=0)")
    
    # 奨学金ありの予測線
    plt.plot(study_grid, mean_pred_1, color="green", label="Bayesian prediction (Scholarship=1)")
    plt.fill_between(study_grid, lower_pred_1, upper_pred_1, color="green", alpha=0.2, label="94% CI (Scholarship=1)")
    
    # 奨学金の有無で実測値を色分け
    scholarship_0 = df[df["Scholarship_True"] == 0]
    scholarship_1 = df[df["Scholarship_True"] == 1]
    
    plt.scatter(scholarship_0["Study_Hours"], scholarship_0["GPA"], 
                color="red", alpha=0.6, label="Observed GPA (Scholarship=0)", s=30)
    plt.scatter(scholarship_1["Study_Hours"], scholarship_1["GPA"], 
                color="orange", alpha=0.6, label="Observed GPA (Scholarship=1)", s=30)
    
    plt.xlabel("Study Hours")
    plt.ylabel("GPA")
    plt.title("Bayesian Regression Lines with 94% Prediction Intervals")
    plt.legend()
    plt.grid(True)
    plt.tight_layout()
    plt.savefig(save_dir + "bayes_prediction_band.png")
    plt.close()


    # ---------------------------
    # 7. 包括的な結果表示
    # ---------------------------
    
    # パラメータの解釈可能性を向上
    print("\n=== モデル結果の解釈 ===")
    print(f"勉強時間の効果 (beta_st): {np.mean(beta_st_vals):.3f} ± {np.std(beta_st_vals):.3f}")
    print(f"奨学金の効果 (beta_ss): {np.mean(beta_ss_vals):.3f} ± {np.std(beta_ss_vals):.3f}")
    print(f"学生間のばらつき (sigma_alpha): {np.mean(sigma_alpha_vals):.3f} ± {np.std(sigma_alpha_vals):.3f}")
    #
    ## 効果量の計算
    effect_size_study = np.mean(beta_st_vals) / np.mean(sigma_alpha_vals)
    effect_size_scholarship = np.mean(beta_ss_vals) / np.mean(sigma_alpha_vals)
    print(f"勉強時間の標準化効果量: {effect_size_study:.3f}")
    print(f"奨学金の標準化効果量: {effect_size_scholarship:.3f}")
    #
    ## 予測精度の詳細評価
    r2 = r2_score(df["GPA"], posterior_mean)
    mae = mean_absolute_error(df["GPA"], posterior_mean)
    print(f"\n=== 予測精度 ===")
    print(f"R²: {r2:.4f}")
    print(f"MAE: {mae:.4f}")
    print(f"MSE: {bayes_mse:.4f}")
    
    # 階層効果の可視化
    plt.figure(figsize=(12, 8))
    
    # 学生別の切片分布
    alpha_means = np.mean(posterior_samples.alpha.values, axis=1)
    alpha_stds = np.std(posterior_samples.alpha.values, axis=1)
    
    plt.subplot(2, 2, 1)
    plt.errorbar(range(len(alpha_means)), alpha_means, yerr=alpha_stds, fmt='o', alpha=0.6)
    plt.xlabel("Student ID")
    plt.ylabel("Random Intercept (α)")
    plt.title("Student-specific Random Intercepts")
    plt.grid(True)
    
    # パラメータの事後分布
    plt.subplot(2, 2, 2)
    plt.hist(beta_st_vals, bins=50, alpha=0.7, label='Study Hours Effect')
    plt.hist(beta_ss_vals, bins=50, alpha=0.7, label='Scholarship Effect')
    plt.xlabel("Parameter Value")
    plt.ylabel("Frequency")
    plt.title("Posterior Distributions of Effects")
    plt.legend()
    plt.grid(True)
    
    # 予測精度の比較
    plt.subplot(2, 2, 3)
    plt.scatter(df["GPA"], posterior_mean, alpha=0.6, label='Bayesian')
    plt.scatter(df["GPA"], lm_preds, alpha=0.6, label='Linear Regression')
    plt.plot([df["GPA"].min(), df["GPA"].max()], 
             [df["GPA"].min(), df["GPA"].max()], 'r--', label='Perfect')
    plt.xlabel("Actual GPA")
    plt.ylabel("Predicted GPA")
    plt.title("Prediction Accuracy Comparison")
    plt.legend()
    plt.grid(True)
    
    # 残差分析
    plt.subplot(2, 2, 4)
    residuals = df["GPA"] - posterior_mean
    plt.scatter(posterior_mean, residuals, alpha=0.6)
    plt.axhline(y=0, color='r', linestyle='--')
    plt.xlabel("Predicted GPA")
    plt.ylabel("Residuals")
    plt.title("Residual Plot")
    plt.grid(True)
    
    plt.tight_layout()
    plt.savefig(save_dir + "comprehensive_results.png", dpi=300, bbox_inches='tight')
    plt.close()
    
    # 結果の要約をCSVに保存
    results_summary = {
        'Metric': ['R²', 'MAE', 'MSE', 'Study_Effect_Mean', 'Study_Effect_Std', 
                  'Scholarship_Effect_Mean', 'Scholarship_Effect_Std', 'Student_Variability'],
        'Value': [r2, mae, bayes_mse, np.mean(beta_st_vals), np.std(beta_st_vals),
                 np.mean(beta_ss_vals), np.std(beta_ss_vals), np.mean(sigma_alpha_vals)]
    }
    
    results_df = pd.DataFrame(results_summary)
    results_df.to_csv("model_results_summary.csv", index=False, encoding='utf-8-sig')

~~~

以下, 個別の部分に関して確認していきます.
特にモデルの設定部分に関して, モデルの定義との対応関係を確認するようにしましょう.

# []{#事前準備}事前準備 {#pymc-data-preparation}

まずは, ライブラリのインポート, データの読み込み, 数量化, 標準化などを実施します.

~~~ py
import pymc as pm
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import arviz as az
import graphviz
from pymc import model_to_graphviz
import statsmodels.api as sm
from sklearn.metrics import mean_squared_error, r2_score, mean_absolute_error
from scipy.stats import norm, rankdata


save_dir = 'images/'

if __name__ == "__main__":

    # ---------------------------
    # 1. データ読み込み
    # ---------------------------
    df = pd.read_csv('hierarchical_regression.csv'
                ,dtype={'GPA': float
                       ,'Scholarship': bool
                       ,'Study_Hours': float
                       ,'Sports_hours': float
                       ,'Part_time_Work': float
                       ,'StudentID':int})

    # 数量化
    df = pd.get_dummies(df, columns=['Scholarship'], dtype='int')

    #標準化
    from sklearn.preprocessing import StandardScaler
    scaler = StandardScaler()
    df[['Scholarship_True'
       ,'Study_Hours'
       ,'Part_time_Work'
       ,'Sports_hours']] = scaler.fit_transform(df[['Scholarship_True'
                                                   ,'Study_Hours'
                                                   ,'Part_time_Work'
                                                   ,'Sports_hours']])

    # ---------------------------
    # ２. 線形回帰
    # ---------------------------
    
    # 説明変数(X)と目的変数(y)に分割
    X = df[['Scholarship_True', 'Study_Hours']]
    y = df['GPA']


    # 切片(定数項)を追加
    X = sm.add_constant(X)
    lm = sm.OLS(y, X).fit()
    print("\n[通常の線形回帰の結果]\n")
    print(lm.summary())
    lm_preds = lm.predict(X)
~~~

::: warn

- `if __name__ == "__main__":` の必要性

このコードブロックでは, メインの処理を `if __name__ == "__main__":` の下に記述しています. これは以下の理由から重要です:

- **モジュールとしてのインポートを防ぐ**: スクリプトを他のファイルからインポートした場合に, メイン処理が自動的に実行されることを防ぎます.

- **PyMC の並列処理との互換性**: PyMC は MCMC サンプリング時に並列処理を行うため, `multiprocessing` モジュールを使用します. `multiprocessing` は各プロセスでスクリプトを再インポートするため, `if __name__ == "__main__":` がないと, 無限再帰や予期しない動作が発生する可能性があります.

- **Windows での実行保証**: 特に Windows 環境では, `multiprocessing` を使用する際に `if __name__ == "__main__":` が必須です. これがないとエラーが発生します.

そのため, ローカル環境でベイズ統計モデリングを行う際は, 必ずメイン処理を `if __name__ == "__main__":` の下に記述するようにしてください.

以降のコードは全て `if __name__ == "__main__":` の下でインデントされている前提となりますので注意してください.

**Google Colab を利用する場合**: Colab のノートブックでは `if __name__ == "__main__":` は不要です. 各セルに直接コードを記述してください. 以降のコード例のインデントを外して, セルごとに実行する形に読み替えてください.

:::


# []{#座標設定}座標設定 {#pymc-coordinates}

まず, `PyMC` でデータを処理するに当たって, **座標 (coordinates)** を定義します.

~~~ py
    coords = {
        "student": df["StudentID"].values,
        "obs_id": np.arange(len(df))
    }
~~~

::: warn

- 座標 (coordinates) の意味

`coords` は, `PyMC` で多次元データや階層モデルを扱う際に使用する座標系の定義です. この辞書は以下のような役割を持ちます:

- **`"student"`**: 学生の ID を表す座標軸です. `df["StudentID"].values` により, 各データポイントがどの学生に対応するかを定義します. これにより, ランダム切片 $\alpha_i$ を学生ごとに定義できるようになります.

- **`"obs_id"`**: 観測値の ID を表す座標軸です. `np.arange(len(df))` により, 各観測値に連番の ID を割り当てます. これにより, データの順序を保持し, 各観測値に対応するパラメータや変数を定義できます.

この座標系により, `PyMC` は学生ごとのパラメータ ($\alpha_i$ など) を適切に管理し, 階層モデルを構築することができます.

:::

# []{#モデル構築}モデル構築 {#pymc-model-construction}

次にモデルを構築していきます. 以下, モデル全体像で定義した各要素と PyMC コードの対応関係を示しながら説明します.

~~~ py
    with pm.Model(coords=coords) as model:
        st = pm.Data("st", df["Study_Hours"])
        #ptw = pm.Data("ptw", df["Part_time_Work"])
        ss = pm.Data("ss", df["Scholarship_True"])
        student_idx = pm.Data("student_idx", df["StudentID"])

        #ランダム切片
        z_alpha = pm.Normal("z_alpha", 0, 1, dims="student")
        mu_alpha = pm.Normal("mu_alpha", 0, 1)
        sigma_alpha = pm.HalfNormal("sigma_alpha", 1)
        alpha = pm.Deterministic("alpha", mu_alpha + sigma_alpha * z_alpha, dims="student")


        # 勉強時間の効果
        beta_st = pm.Normal("beta_st",0,1)
        
        # 奨学金の効果
        beta_ss = pm.Normal("beta_ss",0,1)
 
        mu = alpha[student_idx] + beta_st * st +  beta_ss * ss

        #sigma = pm.HalfNormal("sigma", 1)
        sigma = 0.3
        gpa_obs = pm.Normal("GPA",
                            mu=mu,
                            sigma=sigma,
                            observed=df["GPA"],
                            dims="obs_id"
                        )
        trace = pm.sample(draws =2000, tune=2000, chains=4, target_accept=0.95, return_inferencedata=True)
        pm.sample_posterior_predictive(trace, extend_inferencedata=True)
~~~

## モデル全体像との対応関係 {#モデル全体像との対応関係}

| モデル全体像 | PyMC コード | 説明 |
|------------|-----------|------|
| $y_i \sim N(\mu_i, \sigma^2)$ | `gpa_obs = pm.Normal("GPA", mu=mu, sigma=sigma, observed=df["GPA"])` | 目的変数の分布 |
| $\mu_i = \alpha_i + \beta_{st} \cdot S_t + \beta_{ss} \cdot S_s$ | `mu = alpha[student_idx] + beta_st * st + beta_ss * ss` | 線形予測子 |
| $\alpha_i \sim N(\mu_{\alpha}, \sigma_{\alpha}^2)$ | `alpha = pm.Deterministic(..., mu_alpha + sigma_alpha * z_alpha, dims="student")` | ランダム切片 |
| $\mu_{\alpha} \sim N(0, 1)$, $\sigma_{\alpha} \sim HN(1)$ | `mu_alpha = pm.Normal(0, 1)`, `sigma_alpha = pm.HalfNormal(1)` | 超事前分布 (α) |
| $\beta_{st} \sim N(0, 1)$ | `beta_st = pm.Normal("beta_st", 0, 1)` | 勉強時間の効果 (固定効果) |
| $\beta_{ss} \sim N(0, 1)$ | `beta_ss = pm.Normal("beta_ss", 0, 1)` | 奨学金の効果 (固定効果) |

::: note

- コードの詳細説明

- **非中心パラメータ化**: `alpha = mu_alpha + sigma_alpha * z_alpha` という形式は, **非中心パラメータ化 (non-centered parameterization)** と呼ばれます. これにより, $z_{\alpha} \sim N(0, 1)$ から $\alpha_i \sim N(\mu_{\alpha}, \sigma_{\alpha}^2)$ を生成します. この方法は, MCMC サンプリングの効率を向上させることがあります.

- **`dims="student"`**: ランダム切片 $\alpha_i$ は学生ごとに異なるため, `dims="student"` を指定して学生の座標軸に沿って定義します.

- **`alpha[student_idx]`**: 各観測値に対応する学生の切片を取得します. `student_idx` は各観測値がどの学生に対応するかを示すインデックスです.

- **`beta_st` と `beta_ss` は固定効果**: 現在のモデルでは, `beta_st` と `beta_ss` は学生間で同じ値を持つ固定効果として扱われています. これらは階層構造を持たず, 直接 $N(0, 1)$ の事前分布から推定されます.

- **`observed=df["GPA"]`**: 観測データを指定することで, 尤度関数が定義されます.

:::

