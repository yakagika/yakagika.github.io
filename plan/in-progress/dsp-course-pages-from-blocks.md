---
plan_id: dsp-course-pages-from-blocks
status: in-progress
created: 2026-11-23
updated: 2026-11-23
priority: high
next_actor: agent
next_action: "codex の 2 本 (dsp-course-pages の feedback, dsp-reg-selection-split の feedback で A) を受領して検査し, /tmp の写しで dsp を開いた第 3 回と第 4 回の見本を撮影して本人に見せ, 切り替えの時期を問う"
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

## 実装中に分かったこと (2026-11-23)

- reg-selection の分割は codex が止めた (STOP). 必修の「ステップワイズ法」(reg-selection.md:151 付近) が AIC を採否と停止の基準に使い, 必修の reg-selection-gpa (133 行) も AIC の数え方 $p=k+2$ を other-methods の節へのリンクで参照する. AIC の定義と式は移す予定の other-methods の小節「情報量規準」(約 1.4k 字) にしか無い. 文字数: 残す 3 節 6.1k, other-methods 3.4k (うち情報量規準 1.4k), 経緯 1.3k, 選んだ後の推論 1.0k, 文献 0.7k, 演習 1.5k. 本人に分け方を問う.
- 講義のページの仕組みも codex が実装前に止めた. 仕様の穴 2 点 (同じ必修のブロックが複数の回に出るときのリンクの行き先, 番号付きの回に属さない講義専用ページの講義の順での位置) を coordinator が決めて feedback で返した (`briefs/course-pages-fb1.md`). 行き先は同じ回 → 最初に必修に置く回 → 参考資料の順. 講義の順は目次 → 目次の materials → 各回 (回のページ → materials → 講義のページ) → 選択資料 → 目次で, 講義専用ページは初出の位置だけ.
- codex の調べでは, 第 3 回と第 4 回の必修 7 ブロックに, 参考資料のページ分けを前提にした言い回し (「前のページ」など) は無い.

### 本人の回答 (2026-11-23, AskUserQuestion)

- reg-selection の分け方: 原文「取り敢えずAとして,情報実践の内容の必修,非必修は全て一度再点検します.」 A = 小節「情報量規準」を必修の reg-selection に残し (ステップワイズ法の前に最上位の節 `information-criteria` として置く), 残りの手法, 経緯, 選んだ後の推論, 文献を発展のブロック reg-selection-more へ移す. 第 5 回の intro の授業で扱う項目に情報量規準を加える. 本人が講義全体の必修と発展の分け方を点検し直すので, 分割で見つかった依存 (必修が発展にしか説明の無い用語を使う箇所) は止めずにリンクで指し, 一覧を点検の材料として本人に渡す.

## 対象ファイル (案 1 を採るとき)

`src/Block*.hs` (回のページの下にページを作る, 本文の組み込み, id の区別, リンクの行き先), `templates/block-session.html` と新しい template, `courses/dsp.yaml` (ページのまとめ方の指定), `scripts/test_block_site.py`, `fixtures/`. 切り替えの時に `lectures/dsp/dsp1.md` の発展の説明, 対応表, CLAUDE.md の dsp の節.

## リスク

- 今学期の途中で切り替えると, 学生が持っている旧ページの URL とアンカー (LMS の告知など) が古くなる. 旧ページを残して冒頭に案内を置けば壊れない.
- 回帰分析のブロックは閉じている (本人の通読待ち) ので, 第 5 回は開くまで旧ページを使う.
- 本文を組み込むと, ブロックの中で前のページを前提にした書き方 (「前のページで」など) が 1 ページの中で不自然になることがある. 見本の回で確かめる.

## 変更履歴

- 2026-11-23: 起草 (coordinator). 現状は main で確かめた.
- 2026-11-23: 本人の回答 4 つを記録 (すべて推奨の案).
- 2026-11-23: codex へ 2 本を委譲. 仕組み (依頼文 `_scratch/dsp-layout/handoff-1122/briefs/course-pages.md`, slug dsp-course-pages, effort high) と reg-selection の分割 (`briefs/reg-selection-split.md`, slug dsp-reg-selection-split. 新しい発展のブロック reg-selection-more へ 4 節を移す). 設計の決定: 講義のページの URL は `courses/<course>/<n>/<最初のブロック名>.html`, まとめ方は回の `pages:` (省略時は 1 ブロック 1 ページ), 2 つ目以降のブロックの重なる id は `<ブロック名>-<id>`, 講義の順の移動は 目次 → 回 → 講義専用ページ (初出だけ) → 講義のページ → 次の回. 見本は 2 ブロックをまとめたページを含む第 3 回も見せる (第 4 回は 1 ブロック 1 ページだけのため).
