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


save_dir = '../../images/slds/ch12/'

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
        posterior_predictive = pm.sample_posterior_predictive(trace)

    # ---------------------------
    # 4. モデル診断と可視化
    # ---------------------------
    graph = model_to_graphviz(model)
    graph.render(filename= save_dir + "hierarchical_bayes_model", format="pdf")

    az.plot_trace(trace
                 ,var_names=["sigma_alpha"
                            ,"alpha"
                            ,"beta_st"
                            ,"beta_ss"])
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
    idata = trace.copy()
    idata.extend(posterior_predictive)
    az.plot_ppc(idata, data_pairs={"GPA": "GPA"})
    plt.savefig(save_dir + 'ppc.png')
    plt.close()


    #------------------------------------------------------------------
    # 予測値の可視化
    #------------------------------------------------------------------

    posterior_mean = idata.posterior_predictive["GPA"].mean(dim=("chain", "draw")).values
    bayes_rmse = mean_squared_error(df["GPA"], posterior_mean)
    print(f"[階層ベイズモデルのRMSE] {bayes_rmse:.4f}\n")
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
    print(f"RMSE: {bayes_rmse:.4f}")
    
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
        'Metric': ['R²', 'MAE', 'RMSE', 'Study_Effect_Mean', 'Study_Effect_Std', 
                  'Scholarship_Effect_Mean', 'Scholarship_Effect_Std', 'Student_Variability'],
        'Value': [r2, mae, bayes_rmse, np.mean(beta_st_vals), np.std(beta_st_vals),
                 np.mean(beta_ss_vals), np.std(beta_ss_vals), np.mean(sigma_alpha_vals)]
    }
    
    results_df = pd.DataFrame(results_summary)
    results_df.to_csv("model_results_summary.csv", index=False, encoding='utf-8-sig')
    