# -*- coding: utf-8 -*-
"""
特別講義DS 補足C「YouTube Data API による動画・チャンネルデータの取得」の配布コード (1/2).

市川市に関する動画を数件取得し, 各動画の タイトル・ID・統計 (再生数・高評価数・コメント数)・
コメントを 1 つの CSV (ichikawa_youtube.csv) にまとめる. CSV は 1 行 = 1 コメントで, 動画の
メタ情報を各行に持たせる (発展節の感情分析でそのまま使える形).

事前準備:
  uv add google-api-python-client pandas
  export YOUTUBE_API_KEY=...      # 取得した API キー (または下の API_KEY を直接書き換え)
実行:
  python build_dataset.py         # → ichikawa_youtube.csv
"""

import os
import pandas as pd
from googleapiclient.discovery import build

API_KEY    = os.environ.get('YOUTUBE_API_KEY', 'YOUR_API_KEY')
QUERY      = '市川市'              # 取得したいキーワード
MAX_VIDEOS = 10
OUTPUT_CSV = 'ichikawa_youtube.csv'

youtube = build('youtube', 'v3', developerKey=API_KEY)


def search_videos(query, max_results=10):
    """キーワードで動画を検索し, video_id/title/published_at の DataFrame を返す."""
    res = youtube.search().list(
        part='snippet', q=query, type='video',
        maxResults=max_results, relevanceLanguage='ja').execute()
    return pd.DataFrame([
        {'video_id'    : it['id']['videoId'],
         'title'       : it['snippet']['title'],
         'published_at': it['snippet']['publishedAt']}
        for it in res.get('items', [])
    ])


def get_video_stats(video_ids):
    """動画 ID 群の統計 (タイトル・再生数・高評価数・コメント数) を DataFrame で返す."""
    rows = []
    for i in range(0, len(video_ids), 50):
        chunk = video_ids[i:i + 50]
        res = youtube.videos().list(
            part='snippet,statistics', id=','.join(chunk)).execute()
        for it in res.get('items', []):
            s = it.get('statistics', {})
            rows.append({
                'video_id'     : it['id'],
                'title'        : it['snippet'].get('title', ''),
                'view_count'   : int(s.get('viewCount', 0)),
                'like_count'   : int(s.get('likeCount', 0)),
                'comment_count': int(s.get('commentCount', 0)),
            })
    return pd.DataFrame(rows)


def get_video_comments(video_id, max_comments=100):
    """1 本の動画のトップレベルコメントを DataFrame で返す."""
    rows = []
    request = youtube.commentThreads().list(
        part='snippet', videoId=video_id,
        maxResults=100, textFormat='plainText', order='relevance')
    while request is not None and len(rows) < max_comments:
        response = request.execute()
        for item in response.get('items', []):
            c = item['snippet']['topLevelComment']['snippet']
            rows.append({
                'video_id'            : video_id,
                'comment'             : c['textDisplay'],
                'comment_like_count'  : int(c.get('likeCount', 0)),
                'comment_published_at': c['publishedAt'],
            })
        request = youtube.commentThreads().list_next(request, response)
    return pd.DataFrame(rows[:max_comments])


def main():
    df_search = search_videos(QUERY, MAX_VIDEOS)
    video_ids = df_search['video_id'].tolist()
    print(f'{len(video_ids)} 件の動画を取得: {video_ids}')

    df_stats = get_video_stats(video_ids)

    frames = []
    for _, video in df_stats.iterrows():
        try:
            cdf = get_video_comments(video['video_id'], max_comments=100)
        except Exception as e:
            print(f"skip {video['video_id']}: {e}")   # コメント無効化などはスキップ
            continue
        if cdf.empty:
            continue
        cdf['title']         = video['title']
        cdf['view_count']    = video['view_count']
        cdf['like_count']    = video['like_count']
        cdf['comment_count'] = video['comment_count']
        frames.append(cdf)

    df = pd.concat(frames, ignore_index=True)
    df = df[['video_id', 'title', 'view_count', 'like_count', 'comment_count',
             'comment', 'comment_like_count', 'comment_published_at']]
    df.to_csv(OUTPUT_CSV, index=False, encoding='utf-8-sig')
    print(f'{OUTPUT_CSV} に {len(df)} 行 ({df["video_id"].nunique()} 動画) を保存しました.')


if __name__ == '__main__':
    main()
