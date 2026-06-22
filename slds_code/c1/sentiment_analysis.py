# -*- coding: utf-8 -*-
"""
特別講義DS 補足C の配布コード (2/2).

build_dataset.py が作った ichikawa_youtube.csv を読み込み, 各コメントを日本語 BERT
(christian-phu/bert-finetuned-japanese-sentiment) でネガポジ判定し,
  - sentiment-pie.png     : 1 本の動画の感情比率 (円グラフ)
  - sentiment-scatter.png : 動画横断のポジ率 × 高評価数 (散布図)
を保存する. API キーは不要 (配布 CSV だけで動く).

事前準備:
  uv add pandas matplotlib matplotlib-fontja transformers fugashi unidic-lite torch tqdm
実行:
  python sentiment_analysis.py    # → sentiment-pie.png, sentiment-scatter.png
"""

import os
import re
import torch
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib_fontja  # noqa: F401  日本語ラベルの文字化け防止
from transformers import pipeline, AutoModelForSequenceClassification, BertJapaneseTokenizer
from tqdm import tqdm

INPUT_CSV      = 'ichikawa_youtube.csv'
MODEL_NAME     = 'christian-phu/bert-finetuned-japanese-sentiment'
OUT_DIR        = os.path.dirname(os.path.abspath(__file__))
LABEL_TO_SCORE = {'POSITIVE': 1, 'NEUTRAL': 0, 'NEGATIVE': -1}


def load_classifier():
    if torch.backends.mps.is_available():
        device = torch.device('mps')      # Mac の GPU
    elif torch.cuda.is_available():
        device = torch.device('cuda:0')   # NVIDIA GPU
    else:
        device = torch.device('cpu')
    print(f'device: {device}')
    model = AutoModelForSequenceClassification.from_pretrained(MODEL_NAME)
    tokenizer = BertJapaneseTokenizer.from_pretrained(MODEL_NAME)
    return pipeline('sentiment-analysis', model=model, tokenizer=tokenizer, device=device)


def classify_comments(df, classifier):
    """comment 列を分類し, Label と Score を付与した DataFrame を返す."""
    df = df[df['comment'].apply(lambda x: isinstance(x, str))].copy()
    labels, scores = [], []
    for text in tqdm(df['comment']):
        try:
            r = classifier(text, truncation=True)[0]
            labels.append(r['label'].upper())     # POSITIVE / NEGATIVE / NEUTRAL に正規化
            scores.append(round(r['score'], 4))
        except Exception:
            labels.append('ERROR')
            scores.append(0.0)
    df['Label'] = labels
    df['Score'] = scores
    return df


def main():
    df = pd.read_csv(INPUT_CSV)
    classifier = load_classifier()
    df = classify_comments(df, classifier)

    # --- 1 本の動画の感情比率 (sentiment-pie.png) ---
    one = df[df['video_id'] == df['video_id'].iloc[0]]
    title = re.sub(r'[^\w\s]', '', one['title'].iloc[0])[:18]   # 絵文字・記号を除いて短縮
    plt.figure()
    one['Label'].value_counts().plot.pie(autopct='%1.1f%%', ylabel='')
    plt.title(f'コメントの感情比率（{title}）')
    plt.tight_layout()
    plt.savefig(os.path.join(OUT_DIR, 'sentiment-pie.png'), dpi=150)
    plt.close()

    # --- 動画横断: ポジ率 × 高評価数 (sentiment-scatter.png) ---
    df['signed'] = df['Label'].map(LABEL_TO_SCORE).fillna(0) * df['Score']
    agg = df.groupby('video_id').agg(
        title          = ('title', 'first'),
        n_comments     = ('comment', 'size'),
        positive_ratio = ('Label', lambda s: (s == 'POSITIVE').mean()),
        mean_score     = ('signed', 'mean'),
        view_count     = ('view_count', 'first'),
        like_count     = ('like_count', 'first'),
        comment_count  = ('comment_count', 'first'),
    ).reset_index()

    print('=== 相関 ===')
    print(agg[['positive_ratio', 'mean_score', 'view_count', 'like_count', 'comment_count']].corr())

    plt.figure()
    plt.scatter(agg['positive_ratio'], agg['like_count'])
    plt.xlabel('ポジティブコメント比率')
    plt.ylabel('高評価数')
    plt.title('コメントのポジ率と高評価数の関係')
    plt.tight_layout()
    plt.savefig(os.path.join(OUT_DIR, 'sentiment-scatter.png'), dpi=150)
    plt.close()

    print('saved: sentiment-pie.png, sentiment-scatter.png')


if __name__ == '__main__':
    main()
