---
plan_id: slds-b1-x-api-limit-refresh
status: proposed
created: 2026-08-19
updated: 2026-08-19
priority: medium
next_actor: agent
next_action: "「設計」の 3 点の修正可否をユーザに確認 (AUQ) → 承認された分を lectures/slds/slds_b1.md に反映し, 数値は volatility を織り込んだ書き方に変える"
---

# 補足B (X API) の取得数制限まわりの記述を最新化する

## メタ情報

- **状態**: proposed
- **作成日**: 2026-08-19
- **最終更新**: 2026-08-19
- **next_action**: 下記「設計」の 3 点の修正可否をユーザに確認 (AUQ) → 承認された分を `lectures/slds/slds_b1.md` に反映し, 数値は volatility を織り込んだ書き方に変える
- **next_actor**: agent (ただしユーザ承認が前提)

## 概要

2026-08-19 に X API 従量課金 (pay-per-use) の取得数制限を docs.x.com の一次情報で再調査した結果,
`lectures/slds/slds_b1.md` の記述と現行仕様に 3 点の差分が見つかった. 本計画はその反映.

## 動機

補足Bは 2026-02 の従量課金改定を前提に全面改稿した (commit 1eac99e) が, その後 docs 側が変更されている.
特に月次上限は **告知 (changelog) なしに引き上げられていた**ため, 記載値がそのまま古くなる性質を持つ.

## 調査結果 (2026-08-19 時点, 一次情報 = docs.x.com の raw markdown)

- 月次上限: "Pay-per-usage plans are capped at **3 million Post reads per monthly billing cycle**."
  (`/x-api/getting-started/pricing`, `/x-api/fundamentals/post-cap`, `llms-full.txt` の 3 経路で一致)
  - 対象は Post read のみ. 日次 dedup (同日中の同一 post は 1 件). 失敗リクエストは非課金・非カウント.
  - 超過時は **429** + error type `https://api.x.com/2/problems/usage-capped`. 超えるには Enterprise.
  - cap が app 単位か account 単位かは docs に明記なし (課金 tracking は app level と記載).
- 履歴: Wayback で **2026-03-10 〜 2026-08-12 のスナップショットは一貫して "2 million"**, live は 3 million.
  changelog に該当エントリなし (最新は Aug 13 の Ads API 項目) → 直近 1 週間ほどで無告知に引き上げ.
  2026-02-06 launch 時点の値は当時のスナップショットが JS shell のため未確認.
- リクエスト単位: `search/recent` は 1 回 100 件 (既定 10) / 450 req per 15min (app) / 300 (user).
  `search/all` は 1 回 500 件 / 1 req per sec + 300 per 15min. rate limit と課金は独立.
- full-archive search は **pay-per-use でも利用可** ("Available to pay-per-use and Enterprise customers").
  full-archive 固有の単価は pricing 表になく, post-cap ページが "pricing may vary by data scope" と述べるのみ.
- 残高切れ: docs は "requests will be denied" とのみ. HTTP コードは docs の一覧に **402 が載っていない**が,
  devcommunity では 402 `PaymentRequired` / `CreditsDepleted` の報告が多数 (二次情報).

## 設計 (修正 3 点)

1. **L37 「取得は月200万件で上限」** → 300 万件. あわせて, 数値が無告知で変わる前提の書き方にする
   (例: 「2026-08 時点で月 300 万件. この上限は告知なく改定されるので, 実施前に docs を確認する」).
   Enterprise の「月4万ドル規模」は docs 由来でない (ブログ由来) ため, 出典の弱さに応じて削るか表現を緩める.
2. **L51 「全期間検索には対応した契約が必要」** → 従量課金でも full-archive を使える旨に修正.
   ただし講義の演習では recent (7日) に限定する方針自体は維持し, 「使えるが単価と件数の管理が要る」に書き換える.
3. **L211/L217 「403 = 残高不足」** → 残高切れは 402 系, cap 超過は 429 (`usage-capped`), 403 は
   `client-not-enrolled` (Project 未所属) と切り分ける. 402 の根拠は二次情報である点をどう扱うか要判断.

## 未解決事項 / リスク

- 402 の扱い: 一次情報 (docs の response codes) に 402 が無い. 教材に書くなら「docs に記載がないが実務上 402 が返る」
  という但し書きが要るか, 単に「残高が尽きるとリクエストが拒否される」と HTTP コードを書かないか.
- 補足B L27 の「Basic/Pro は新規受付終了」: 2026-02-06 の launch 告知は "Basic and Pro plans remain available"
  と書いており, 現況と食い違う可能性がある. 今回の調査 scope 外なので未検証.
- 数値の陳腐化: 上限は 5 ヶ月で 2M→3M と変わった. 講義資料に生数値を書く限り再発するので, 書き方の方針を決める.
- **反映先が未確定 (2026-09-04 追記)**: 本計画は起票時 (2026-08-19) の想定どおり `lectures/slds/slds_b1.md`
  を対象にしているが, その後 slds は凍結され (CLAUDE.md §dsp 講義, 例外は featured=false と既知の誤記修正のみ),
  新科目 `lectures/dsp/` へ複製された. 3 点の修正が「既知の誤記」に当たるかは判断が要る.
  当たらないなら反映先は dsp 側の補足B になる. 着手前にどちらへ当てるかを決める.

## 関連ファイル

- `lectures/slds/slds_b1.md` (L27-51, L211-217, L346-352)
- `lectures/slds_code/b1/` (fetch_posts.py の HARD_CAP / 単価定数)

## 出典

- https://docs.x.com/x-api/getting-started/pricing
- https://docs.x.com/x-api/fundamentals/post-cap
- https://docs.x.com/x-api/fundamentals/rate-limits
- https://docs.x.com/x-api/fundamentals/response-codes-and-errors
- https://docs.x.com/x-api/posts/search/introduction
- https://docs.x.com/changelog
- https://web.archive.org/web/20260310044759/https://docs.x.com/x-api/getting-started/pricing (2 million)

## 変更履歴

- 2026-08-19: 作成 (調査セッションの結果を反映)
