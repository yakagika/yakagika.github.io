---
plan_id: dsp-course-pages-from-blocks
status: in-progress
created: 2026-11-23
updated: 2026-11-23
priority: high
next_actor: agent
next_action: "仕組み (講義のページへの本文の組み込み, ページのまとめ方の指定, id の区別, リンクの行き先, 講義の順の移動) と reg-selection の分割の依頼文を書いて codex へ委譲し, 第 4 回の見本を作って本人に見せる"
---

# dsp の講義のページを参考資料のブロックから組み, 発展は参考資料へのリンクにする

## メタ情報

- 状態: 方針は決定 (2026-11-23 本人の回答). 仕組みの実装と見本の作成に進む. 親計画は [lecture-block-restructure](lecture-block-restructure.md) (Phase 1 で決めた「回のページ = ブロックへのリンクの一覧」と Phase 5 の切り替えを変える).
- 本人の依頼 (2026-11-23, 原文): 「現行のデータサイエンス実践の発展の扱いはどの様になっていますか? 発展項目は教科書へのリンクにして講義資料からは省いても良いように思います. また,講義資料の1ページを今のサイズに固定する必要はないかと思います. 利用しているブロック単位あるいは複数のブロック単位でまとめて,1ページの量を減らす案を作成して下さい.」

## 現状 (2026-11-23 に main で確かめた)

### 発展の扱い

- **旧講義ページ (`lectures/dsp/dspN.md`. 今学期の授業で使用中)**: 発展は章の中に節として置き, 見出しに「(発展)」を付ける. 30 節 (Ch3 8, Ch4 1, Ch5 5, Ch6 3, Ch7 2, Ch9 5, Ch10 1, Ch11 5). 加えて Ch10 は 1 節を除いて, Ch13〜16 は章ごと授業で扱わない. dsp1 の「発展の節」と「発展の節の一覧」が学生向けの説明と一覧. 決まり: 授業時間では扱わず, 資料には書いて自習に回す (2026-10-05 に「課題の選択肢」から変更. 対応表の凡例). 演習の文面から発展の節を参照するものがある (例 DSP3-3 が for 文のグラフとグラフの分割を参照).
- **参考資料 (`fields/`) と講義の並び (`courses/dsp.yaml`. 講義は閉じている)**: 参考資料の本文には「(発展)」と書かない. 必修と発展はブロック単位で講義の並びに `level: required | advanced` と書く. 旧ページの発展の 30 節は, 移行のときにほぼすべて発展のブロック (vis-design, vis-more, num-partial, reg-ols, reg-selection-lab, reg-glm, reg-logit, ml-classifiers, ml-clustering-more, nlp-generation, nlp-embedding-more, dl-training-case, dl-training-procedure, img-more, img-age-code) に入った. 例外は reg-selection (必修) で, 自習用の節 (分野ごとの経緯, 選んだ後の推論, 文献) が混ざり, 回の intro の文で「自習用」と断っている. 選択の章 (ベイズ, 質問紙, 欠損値, 因果) は「選択資料」として発展のブロックだけを並べる.
- **回のページ (`templates/block-session.html`)**: 本文は持たず, 回の intro と, 必修と発展のブロックへのリンクを 1 つの一覧に「(必修 / code)」「(発展 / theory)」の形で並べ, 演習と提出 file 名を添える. 学生は参考資料のページ (ブロックを最上位の見出しで分けたページ) を読む.
- **対応表** (`plan/landed/2026-11-20-dsp-curriculum-matrix.md`): オプション項目のうち 5 件を発展に置き, 節アンカーは旧ページ (`dspN.html#...`) を指す.

### ページの大きさ (source の文字数. コードと記法を含む)

- 旧ページ 1 章: dsp3 73k, dsp4 44k, dsp5 81k, dsp6 30k, dsp7 39k, dsp8 27k, dsp9 70k, dsp10 45k, dsp11 81k.
- ブロック: 1.5k〜27k. 必修のブロックは ml-clustering (26.9k) を除いて 21k 以下.

## 案

1. **講義のページはブロックの本文を組み込む**. 回ごとに, 必修のブロックを講義の順に 1 ページ 1 つ (小さいものは隣と 2 つまで) 並べ, 本文は build 時に参考資料のブロックから組み込む (写しを作らない. 正本は参考資料のまま). 1 ページの目安は 2 万字以内.
2. **発展は講義のページから外し, 参考資料へのリンクにする**. 回のページと, 各回の最後のページの末尾に「発展 (参考資料で読む)」の一覧を置く. 必修のブロックに混ざる自習の節 (reg-selection) は, ブロックを分けて発展のブロックにする (推奨) か, 講義の並びで節を除く指定を足す.
3. **移動は旧講義ページと同じ形**: 左の欄 = 講義の目次 (回 → ページ → 見出し), 本文の上下 = 講義の順の移動カード.
4. **本文の中のリンク**: `block:` のリンクは, 行き先が同じ講義の必修のブロックなら講義のページへ, それ以外 (発展, 他の分野) は参考資料へ.
5. **まとめたページの id**: `exercises` と `演習` の id が 31 ブロックにあり, まとめると重なる. まとめたページでは重なる id にブロックの名前を付けて区別し, build で衝突を検査する. 1 ブロック 1 ページなら起きない.
6. **同じ本文が 2 つの URL に出る**: 講義のページに参考資料の該当ページを指す canonical を付ける.
7. 対応表の節アンカーは切り替えの時に講義のページ (必修) か参考資料 (発展) へ付け替える. 演習の番号 (DSPn-k) と提出 file 名の改名は別の課題 (親計画で「当面は現行のまま」) で, この案では変えない.

### ページ割りの案 (案 1 の目安で組んだもの)

| 回 | 旧ページ | 講義のページ (必修) | 発展 (参考資料へのリンク) |
|---|---|---|---|
| 2 分析設計 | dsp2 8k | design-process 7k | — |
| 3 可視化 | dsp3 73k | vis-basic 20k / vis-distribution 18k / vis-relation + vis-reading 18k | vis-design, vis-more |
| 4 数値化 | dsp4 44k | num-summary 15k / num-correlation 21k / num-distance 5k | num-partial |
| 5 回帰分析 (閉じている) | dsp5 81k | reg-intro + reg-simple 15k / reg-multiple + reg-gpa 16k / reg-prediction + reg-evaluate 15k / reg-causal 8k / reg-selection 14k / reg-selection-gpa 11k | reg-ols, reg-selection-lab, reg-glm, reg-logit |
| 6 機械学習 | dsp6 30k | ml-overview 9k / ml-classification 16k | ml-classifiers |
| 7 クラスタリング | dsp7 39k | ml-clustering 27k / ml-association 7k | ml-clustering-more |
| 8 NN と Transformer | dsp8 27k + dsp10 の 1 節 | ml-network 14k / dl-transformer + dl-lifecycle 17k | — |
| 9 自然言語処理 | dsp9 70k | nlp-processing 16k / nlp-syntax 9k / nlp-bert 15k / nlp-embedding 13k | nlp-generation, nlp-embedding-more, dl-training-case, dl-training-procedure |
| 10 画像認識 | dsp11 81k | img-recognition + img-cnn 22k / img-age + img-speech 19k | img-more, img-age-code |

- 1 回あたり 1〜6 ページ, 1 ページ 5k〜27k. 2 万字を超えるのは ml-clustering (1 ブロックで 27k) と img-recognition + img-cnn (22k). 前者はブロックを分ける候補.
- 総合実習 (第 11〜13 回) と選択資料は今の形 (講義専用ページと参考資料へのリンク) のまま.

## 本人に問うこと

1. 講義のページの形: ブロックの本文を組み込む (推奨) / 今の仕組み (回のページはリンクの一覧で, 学生は参考資料のページを読む) のまま発展を別の欄に分けるだけ.
2. ページの単位: 1 ブロック 1 ページを基本に小さいものだけまとめる (推奨, 上の表) / 常に 1 ブロック 1 ページ / 1 回を 2〜3 ページに大きくまとめる.
3. 必修のブロックに混ざる自習の節 (reg-selection): ブロックを分ける (推奨) / 講義の並びで節を除く.
4. 切り替えの時期: 仕組みを作り, 見本の回 (第 4 回) を本人が見てから決める (推奨) / 今学期の残りの回から新しいページに切り替える / 来年度の秋学期 (親計画の Phase 5) に切り替える.

## 本人の回答 (2026-11-23, AskUserQuestion)

1. 講義のページの形: ブロックの本文を組み込む.
2. ページの単位: 1 ブロック 1 ページを基本にし, 小さいものだけ 2 つまとめる (上の表の割り方).
3. reg-selection の自習の節: ブロックを分ける (自習の節を発展のブロックへ移す. 参考資料の URL が変わる分は旧 ID の対応を付ける).
4. 切り替えの時期: 仕組みを作り, 第 4 回 (データの数値化) の見本を本人が見てから決める. 旧ページはそれまでそのまま.

## 対象ファイル (案 1 を採るとき)

`src/Block*.hs` (回のページの下にページを作る, 本文の組み込み, id の区別, リンクの行き先), `templates/block-session.html` と新しい template, `courses/dsp.yaml` (ページのまとめ方の指定), `scripts/test_block_site.py`, `fixtures/`. 切り替えの時に `lectures/dsp/dsp1.md` の発展の説明, 対応表, CLAUDE.md の dsp の節.

## リスク

- 今学期の途中で切り替えると, 学生が持っている旧ページの URL とアンカー (LMS の告知など) が古くなる. 旧ページを残して冒頭に案内を置けば壊れない.
- 回帰分析のブロックは閉じている (本人の通読待ち) ので, 第 5 回は開くまで旧ページを使う.
- 本文を組み込むと, ブロックの中で前のページを前提にした書き方 (「前のページで」など) が 1 ページの中で不自然になることがある. 見本の回で確かめる.

## 変更履歴

- 2026-11-23: 起草 (coordinator). 現状は main で確かめた.
- 2026-11-23: 本人の回答 4 つを記録 (すべて推奨の案).
