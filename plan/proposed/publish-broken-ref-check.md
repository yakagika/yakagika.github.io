---
plan_id: publish-broken-ref-check
status: proposed
created: 2026-10-28
updated: 2026-10-28
priority: medium
next_actor: user
next_action: "本人が下の設計 (検査の範囲, 止め方) を承認したら, agent が隔離 worktree で検査 script と publish.sh の変更を作り, 画像を 1 枚消した copy で検出を確かめてから main へ取り込む"
---

# 公開前に参照切れを検査する

## メタ情報

- **状態**: proposed
- **作成日**: 2026-10-28
- **出所**: 本人の裁定 (2026-10-28, 「入れる」). 2026-04 の画像整理 (3f73931) が, 本文が参照している画像 5 枚を消していた (57f48b0 で復元).

## 原因

画像整理のとき, 参照の有無を Markdown の正規表現で判定していた. 説明文が複数行にまたがる参照や入れ子の角括弧を含む参照を拾えず, 参照のある画像を「参照なし」と判定した. 公開の手順 (`publish.sh`) には, 生成物の参照先が実在するかを確かめる段がない. そのため消えた画像は公開後も気づかれなかった.

## 設計

- 検査は Markdown でなく, Hakyll が生成した `docs/**/*.html` を HTML parser で読んで行う. pandoc が解釈した後の参照を見るので, 記法の揺れに左右されない.
- 対象: `img`, `source`, `video`, `audio`, `embed` の `src`, `object` の `data`, `a` の `href` のうち, scheme の無い (サイト内の) 参照. 参照先の file が `docs/` に無ければ, ページと参照を列挙して終了コード 1 を返す. `#id` のページ内 anchor は今回は見ない.
- 置き場所: `scripts/check_local_refs.py` (Python 標準ライブラリのみ).
- `publish.sh` に `set -e` を足し, `stack exec main rebuild` の後, `git add` の前で検査を走らせる. 参照切れがあれば commit と push に進まない.
- 2026-10-28 の試作では, 現在の `docs/` (114 ページ) で参照切れは 0 件だった. 導入しても今の公開は止まらない.

## 検証

- 現在の `docs/` で 0 件を返す.
- `docs/` の copy から画像を 1 枚消し, その画像を参照するページが報告され, 終了コードが 1 になる.

## 未解決事項 / リスク

- 外部 URL の死活は見ない (公開のたびにネットワークへ出るため).
- anchor の検査を足すかは, 導入後に誤検出の量を見て決める.
