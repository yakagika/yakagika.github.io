---
plan_id: dsproj-course-pages
status: in-progress
created: 2026-11-27
updated: 2026-11-27
priority: high
next_actor: agent
next_action: "本人が共通理解を確認したら, 講義の並び courses/dsproj.yaml と講義専用ページ 6 つ, データの取得の分野 (補足 A〜C の移行, codex) に着手する"
---

# データサイエンスプロジェクト (旧 特別講義データサイエンス) の講義のページを作る

## メタ情報

- **状態**: in-progress (2026-11-27 起票. 同日に本人が計画を承認し, grill-me で未決定を詰めた)
- **作成日**: 2026-11-27
- **最終更新**: 2026-11-27
- **関連計画**: [lecture-block-restructure.md](lecture-block-restructure.md) (Phase 4: データサイエンスプロジェクトとゼミの講義のページ, データの取得の分野), [lecture-literature-grounding.md](lecture-literature-grounding.md) (文献の登録と引用), `plan/proposed/slds-b1-x-api-limit-refresh.md` (X の API の料金と制限の更新. 直す先を新しいブロックに変える)
- **出所**: 本人の依頼 (2026-11-27) 「特別講義の資料を改定してデータサイエンスプロジェクトにする (タイトルには旧特別講義データサイエンスと記載)」. 問いの立て方の素材は実学入門 II 第6回の pptx (授業の dir, 32 枚).

## 概要

2027 年度から特別講義 (データサイエンス) はデータサイエンスプロジェクトになる. 技術は他の科目に分かれたので, この講義は研究に集中し, LLM の利用を前提に, 学生本人の問題意識に基づく研究を目指す. 新しい講義の並びと講義専用ページを作り, 技術的な内容は参考資料 (分野のブロック) へのリンクにする.

## 確定した要件 (2026-11-27 本人)

| 項目 | 決定 |
|---|---|
| 置き場所 | 新しい講義の並び `courses/dsproj.yaml` と講義専用ページ `courses/dsproj/*.md`. 旧 slds のページは上書きせず, 今年度の受講生のために残す |
| 題名 | データサイエンスプロジェクト (旧 特別講義データサイエンス) |
| 科目の分離 | プログラミング → データサイエンスのためのプログラミング, 統計 → データ活用の統計学, データ活用の統計学実践, 技法 → データサイエンス実践. この講義は研究に集中する |
| 未履修者 | 他の科目を履修していなくても講義の中で完結するようにする. データ活用の統計学とデータサイエンス実践を同じ年度に履修することを勧める (データサイエンス実践は秋学期, この講義は通年) |
| 技術的な内容 | 参考資料へのリンクだけにし, 学生の目的に応じて講義の中で扱うと明記する |
| 期間と成果物 | 通年. 論文を書き, 学会で発表する. 可能なら夏休みに学内の奨学論文コンテストで発表する |
| 研究の方針 | 特別講義は 1 年で完結することを優先し, 取り組みやすさを重視した計画を立てていた. この講義では LLM の利用を前提にし, LLM なしでは難しいが本人が本当に面白いと思える, 本人の問題意識に基づく研究を目指す. そのため LLM の利用法と問いを立てることに時間を使う |
| 「面白い」の基準 | 学生の問いを掘り下げ, どのように実現できるかを考える. 先行研究との位置づけは考えるが, 厳密な新規性は求めない |
| 扱う範囲 | データサイエンスに属する課題に限る. ABM や人文学的な内容は扱わない (データサイエンス以外の研究はゼミで扱う) |
| ゼミとの関係 | 並行して履修してよく, 研究内容を共通にしてもよい. 共通にする場合は, 調査やデータの収集を始める前 (倫理審査の申請の前) に教員に伝え, 審査の用途にゼミを含める. 卒論, 他の講義, 学外での二重利用と自己剽窃の注意は旧第2章のまま残す |
| LLM の道具と契約 | データサイエンス実践と同じ考え方. Codex, Gemini, Claude のどれでもよく, 推奨はその時期に講義で伝える |
| LLM の線引き | セキュリティを学んだうえで, 学生が内容を理解して人に説明できる範囲で使う. 説明できない成果は, 結果が出ていても採用しない. 理解は少人数の対面で教員と話しながら確かめる. 論文と発表も LLM の出力でよいが, 中身を制御して使えることが前提 |
| 個人に関わるデータ | 調査の回答などの生データは LLM に渡さない. 列の名前と型だけを伝えてコードを書かせ, 手元で実行する. エージェントに作業ディレクトリを任せるときは, データをその外に置く |
| 最初に全員で扱う共通資料 | プログラミング用の設定, git と GitHub, コーディングエージェント. Python の基礎は未履修者が並行して読む. LaTeX は論文と発表の段階, Colaboratory は計算が重いときに読む |
| 書籍の画像 | 書籍のページの画像, 表, 表紙は載せない. 頁を付けた短い引用と, 自分の言葉での説明にする |
| pptx の学生の情報 | 6 枚目 (グループ表, 氏名入り) と 2〜4, 8, 10 枚目 (学生の文) は使わない. 例は作り直す |
| 回の組み方 | 段階ごとに 1 回. 本資料の読み方に「回は段階を表し, 1 回の授業とは限らない」と書く |
| 論文と発表 | 今回は骨子だけ作り「作成中」と明記する. 中身は別の計画で起こす |
| 演習の見出し | `### Exercise DSPJn-k` (n は回, k は連番). アンカー `#exercise-dspjn-k` |
| 参考資料 | 分野のブロックへリンクする. 分野に無い主題は, 分野へ移すまで旧特別講義の章へリンクし, 移したら付け替える |
| データの取得 | 補足 A〜C を分野「データの取得」へ移す (今回の作業に含める) |
| カテゴリーデータ | 調査データの分野へ統合する. 事実の確認で, 旧第16章の内容は調査データの分野の 3 ブロック (survey-design, survey-analysis, survey-sem) に移行済みと分かった. 今回はリンクするだけ |
| 時系列解析 | 新しい分野として足す. 今回の作業に含めない. 2027 年 4 月 (開講) までの作業として再編計画に記録する. それまでは旧第17章へリンクする |
| 検定 | 推測統計学の分野 (未執筆. データ活用の統計学の資料から移す. 再編計画の Phase 3) に統合する. それまでは旧第10章へリンクする |
| 共通資料と参考資料 | 後で統合して整理する (本人. 再編計画に記録する) |

## 設計

### 講義の並び (`courses/dsproj.yaml`)

| 回 | 題 | 講義専用ページ |
|---|---|---|
| 1 | 本資料の読み方と, 特別講義との違い | `guide` (科目の分離, この講義の方針, 年間の流れ, ゼミとの関係, 研究倫理の注意, デザインの説明) |
| 2 | 共通資料 | `common` (設定, git, エージェントを読む順序と, 未履修者の Python の基礎, LaTeX と Colaboratory を読む時期) |
| 3 | LLM を使った研究の進め方 | `llm` (セキュリティとデータの扱い, 問いを広げる相手として使う, 先行研究を探し本文を確かめる, 分析のコードを書かせて確かめる, 説明できる範囲で使う, 利用の開示) |
| 4 | 問いの立て方 | `question` (pptx から. テーマと問い, 自分の問いと副次的なテーマ, 問いの形式, 共通するものを探す, クエスチョンバースト, 3 回の実演, 掘り下げと分析, 答えるための地図) |
| 5 | 研究計画 | `plan` (旧第2章を土台. モデルとデータ, 目的・手法・データの決め方, 過去の研究事例. 「妥協」の部分を逆の方針で書き直す) |
| 6 | 論文と発表 (作成中) | `writing` (骨子. 年間の締切, 論文の構成, LLM の出力を制御して使う, LaTeX の資料へのリンク) |
| 参考資料 | 分野のブロックの一覧 | supplements. 分野に無い主題 (時系列解析, 検定) は intro から旧章へリンク |

### 図

- pptx 11 枚目 (自分の問いと副次的なテーマ・問い) と 25 枚目 (問いを掘り下げる・広げる・分析する) を SVG の概念図で描き直す (`images/courses/dsproj/`).

### データの取得の分野 (`fields/data-acquisition/`)

| ブロック | 元 |
|---|---|
| `api-intro` | 補足 A の「API とは」(REST, HTTP, JSON の一般論) |
| `api-edinet` | 補足 A の残り (末尾の回帰分析は残し, 理論は回帰分析の分野へリンク) |
| `api-x` | 補足 B |
| `api-youtube` | 補足 C (演習の見出し `Exercise SLDSc-k` は, 調査データの分野が DSP15-k を保っているのと同じく保つ) |

- 既存の文章の移行なので codex (Sol) に委譲する. 主戦は受入と build を行う.

### 文献

- マラニー, レア (安原和見 訳)『リサーチのはじめかた』と, クエスチョンバーストの出典 (グレガーセン『問いこそが答えだ!』と思われる. 確かめてから引く) を論文 DB に `blog-lectures` で登録し, `scripts/make_lecture_bib.sh` で bib を作る. 書誌は出版社の情報で確かめる.

### 合わせて直すもの

- 講義一覧 (`pages/lectures.markdown`): 新しい講義のカードを足し, 特別講義のカードは今年度の受講生向けと書く.
- データサイエンス実践の `courses/dsp/guide.md` (「本科目 → データサイエンスプロジェクトの順に履修」) と `scope.md` を, 同じ年度に履修することを勧める書き方に揃える.
- `lectures/common/agent.md` の「この資料を使う講義」.
- `CLAUDE.md` の slds の節 (データサイエンスプロジェクトの講義の並びと演習の見出し).
- 再編計画: Phase 4 の進捗, 時系列解析の分野, 共通資料と参考資料の統合, 旧 slds の削除の前に参考資料の旧章へのリンクを付け替える条件.

## ロードマップ

1. 講義の並びと講義専用ページ 6 つ, 概念図 2 枚, 文献の登録 (主戦が書く. 執筆の前に skill writing-style を発動する).
2. データの取得の分野 (codex Sol, 隔離 clone). 1 と並行.
3. 合わせて直すもの.
4. build, 参照の検査, 引用の検査. 文章は codex に独立に読ませ, 相違を裁定する.
5. 本人の通読. 公開は本人の go を待つ.

## 未解決事項 / リスク

| 問い | 親 | 状態 | 分岐先 | 外へ出した物 | 戻る条件 | 結論 |
|---|---|---|---|---|---|---|
| 論文と発表の中身 | 論文と発表の骨子 | open | 別の計画 | | 骨子を本人が読んだ後 | |
| 時系列解析の分野 | 参考資料 | open | 再編計画 | | 2027-04 まで | それまでは旧第17章へリンク |

## 関連ファイル

- `courses/dsproj.yaml`, `courses/dsproj/*.md`, `images/courses/dsproj/`
- `fields/data-acquisition/`
- `lectures/slds/slds2.md` (研究計画の土台), `lectures/slds/slds_a1.md`, `slds_b1.md`, `slds_c1.md`

## 進捗と再開の手順 (2026-11-27)

- 講義のページは隔離 worktree `.claude/worktrees/dsproj` (branch `feat/dsproj-course`) で書いている. 並行 session が `publish.sh` (`git add --all`) を走らせても書きかけが公開されないようにするため. WIP commit eb729066: `courses/dsproj/{guide,common,llm,question}.md` と `images/courses/dsproj/question-{center,chain}.{svg,png}` (rsvg-convert -w 1920).
- 残り: `courses/dsproj/plan.md` (旧 slds2 を土台. 図 `/images/slds/ch2/{data-science-flow,info-to-model,purpose-method-data}.png` を再利用. 「妥協」の段を逆の方針に. 過去の研究事例のリスト. 演習 DSPJ5-1 事例を読む, DSPJ5-2 目的・データ・手法), `courses/dsproj/writing.md` (骨子, 作成中. 年間の締切, 論文の構成, LLM の出力を制御して使う, LaTeX 資料へのリンク), `courses/dsproj.yaml` (6 回の materials と supplements `id: reference` (出力は `/courses/dsproj/reference.html`). intro で時系列 `/lectures/slds17.html` と検定 `/lectures/slds10.html` へ旧資料としてリンク. blocks は全分野 + data-acquisition).
- 書いたページのリンク: `material:<名前>#id` 形式, `/lectures/agent.html#sandbox-と承認` `#分からないまま承認しない` `#文献の本文確認` `#差分の説明`, `/fields/data-acquisition/index.html` (codex の land 後に存在). 引用キー `mullaney2023research` (p. 35, pp. 98-99), `gregersen2020questions`.
- 文献: papers へ handoff 済み (chain `haskell-blog-2026-11-27T11-03-32-909643-…`, mechanical). papers の待ち受け session は不在で未処理. 公開前に `scripts/make_lecture_bib.sh` で 2 件が出ることを確かめる.
- データの取得の分野: codex (Sol) が完了, STOP なし. clone `~/.isolated-checkouts/ea-codex-blog-data-acquisition` (branch `codex/data-acquisition-field`), 報告は session の task 出力 (bo9239jga). 未受領: 差分の確認 (Sonnet subagent に変更範囲と規約適合を pass/fail で), clone で commit, worktree へ取り込み, build.
- 執筆規約の lock は並行 session の bump を df9f75fc で commit した.
- 合わせて直すもの (未着手): 講義一覧のカード, `courses/dsp/guide.md` と `scope.md` の履修順, `lectures/common/agent.md` の「この資料を使う講義」, CLAUDE.md の slds の節, 再編計画への記録 (時系列の分野, 共通資料と参考資料の統合 (本人 2026-11-27 「計画としてあとで, 共通資料と参考資料を統合して, 整理します.」), 旧 slds 削除前に参考資料の旧章リンクを付け替える条件, X の API の計画の直す先).
- 検証: worktree で main の実行ファイル (`/Users/akagi/Developer/Haskell/blog/.stack-work/install/aarch64-osx/6f1c476cb9aa0d5e2541d62478eb9f2493ff879d0abdf66b4aa56174cdbe416d/9.6.6/bin/main rebuild`) を走らせ, `check_local_refs.py docs`, `check_citations.py`, `prose_lint.py`. 文章は codex に独立に読ませる.

## 変更履歴

- 2026-11-27: 作成 (計画の承認と grill-me の結果を記録)
