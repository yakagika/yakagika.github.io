---
plan_id: learn-layer-personal-knowledge-queue
status: in-progress
created: 2026-06-11
updated: 2026-06-11
next_action: ドラフト (posts/2026-06-11-personal-learn-layer.md) のレビューと公開承認 (publish.sh はユーザ承認後)
next_actor: user
---

# 個人の学習層 (Learn) — Agent が薦め, スマホで少しずつ消化し, vault に集積する

## メタ情報

- **状態**: in-progress (ドラフト作成済, ユーザレビュー待ち)
- **作成日**: 2026-06-11
- **進捗 (2026-06-11)**: `posts/2026-06-11-personal-learn-layer.md` ドラフト作成.
  orchestrator 記事と相互リンク済. スクショ 2 枚を
  `images/posts/personal-learn-layer/` に配置 (inline-qa.png / note-categorical.png).
  `stack build && stack exec main build` 通過, RSS 著者付き確認済.
  残: ユーザレビュー → 公開承認 → publish.sh.
- **発端**: assistant (Orchestrator) セッションで, ユーザ本人の知識獲得・学習層を
  Phase 1 まで構築 (repo: `~/Developer/claude/learn`, データは existence vault の
  `Learn/`). この機構の設計と運用を記事化する依頼.
- **位置づけ**: 既存記事 [Multi-Repo Agent Orchestrator](../posts/2026-06-03-multi-repo-agent-orchestrator.md)
  (2026-06-03) の続編. Orchestrator が「Agent 群を束ねる上位層」なら, 本記事は
  「**人間 (私) 自身の学習を Agent 群と同じ枠組みで回す層**」の話.
- **対象**: 新規 `posts/2026-06-11-personal-learn-layer.md` (または近い日付).

## 何を書くか (主題)

セッション中に Agent が教えた知識・私が学びたいと言った知識・Agent が学習を薦める
知識を **queue** し, スマホ (Obsidian mobile) で M/N 形式で少しずつ消化 → Done →
Obsidian vault に**集積**する仕組みを, 設計判断とともに解説する. 「AI に教わった
ことが流れて消える」問題への, 自前の解 (PKM × Agent orchestration) の記録.

## 反映すべき「先程 learn に投入された queue の中身」

記事は抽象論で終わらせず, 実際に queue に入った 2 件を具体例として使う:

1. **`Learn 層の使い方` (seed, 3 parts)** — 機構そのものを学ぶチュートリアル note.
   「queue と進捗 (M/N)」「非同期 Q&A の書き方」「なぜ Obsidian なのか」の節を,
   記事の UX 説明のスクリーンショット的素材として引用する.
2. **`圏論入門 — 台帳の並列意味論` (ea-scaling-paper 由来, 23 parts, 確認問題 6 問)**
   — 他 repo の Agent が `learn_enqueue.py --recommended-by user` で投入した実例.
   「研究 (ea-scaling-paper §4 の主張を読めるようにする) のために, 長い知識を 23 Part
   に割り, 確認問題つきで自分に配信する」という,**本機構が最も活きるユースケース**
   として中心に据える. 数学知識を初学者向けに段階配信する話は記事の山場になる.

> 注意: 圏論 note の本文そのものは長大なので, 記事には「割り方・確認問題の付け方・
> 巡回が添削を返す流れ」を **数 Part 抜粋**で見せる. 全文転載はしない.

## 記事の構成案 (outline)

1. **動機** — AI との対話で得た知識が流れて消える / 学びたい話題が積まれない問題.
   Orchestrator 記事の「知識層」が *Agent のための* RAG だったのに対し, これは
   *人間のための* 学習キュー, と対比して入る.
2. **要件** — Agent 判断→承認 or 自己指示で queue / スマホで確認 / M/N で少しずつ /
   学習済みは Obsidian に集積 / (任意で) Agent へ質問・音声.
3. **手法比較** — Todoist のみ / Obsidian / 専用アプリ / Anki 等を, メリデメ表で.
   なぜ「Obsidian 正本 + Todoist 薄ミラー」に落ち着いたか (本文置き場・集積要件・
   検証順序・アプリ化への布石).
4. **設計の肝**:
   - **1 ファイル 1 書き手** — iCloud 同期はオフライン編集衝突で片方を無言上書き
     する (実際に別 repo で conflict copy 58 件を踏んだ). queue note = ユーザ専有,
     answers = Agent 専有, dashboard = 生成物, と書き手を固定して構造的に回避.
   - **非同期 Q&A** — note に `❓質問` を書くと毎時巡回 (`learn_patrol.py` →
     `answer_questions.sh`) が拾って `claude -p` で回答を別ファイルに append し,
     note 側は embed で読む. リアルタイム対話なしで「質問できる」を成立させる.
   - **M/N 分割** — 入力 markdown を見出し/段落で Part に割り, 読了 checkbox から
     進捗を導出 (note には書き戻さない).
   - **方式B launchd helper** — iCloud (Mobile Documents) は launchd の /bin/bash
     直起動だと TCC で書けない. 独立コンパイル helper を responsible process に
     する回避策 (backup 機構で確立した手法の再利用).
5. **他 repo Agent からの登録経路** — 専用 tool を作らず, 既存の
   `request_user_action` (📖 prefix) → 対話セッション承認 → `learn_enqueue.py` の
   承認フローに相乗りした話. タスク分離規約 (横断タスクは Orchestrator のみ,
   各 repo は自リポ surface のみ) との整合も触れる.
6. **実例: 圏論 note** — 上記「反映すべき queue 中身」2 を具体的に展開.
7. **段階導入と今後** — Phase 1 (Obsidian コア) は稼働中. Phase 2 (Todoist 通知
   ミラー + `obsidian://` deep link), Phase 3 (TTS / 間隔反復), Phase 4 (アプリ化)
   の見取り図. 「正本が plain markdown だからアプリ化が障害にならない」点で締める.

## tags 案

`claude-code`, `obsidian`, `pkm`, `spaced-learning`, `orchestrator`, `automation`,
`mcp`. orchestrator 記事と揃え, 続編として相互リンクする (featured も検討).

## 受け入れ基準

- orchestrator 記事から本記事へ, 本記事から orchestrator 記事へ相互リンクがある.
- 圏論 note を題材に「長い研究知識を確認問題つきで段階配信する」具体像が伝わる.
- 設計の肝 4 点 (1ファイル1書き手 / 非同期Q&A / M/N / 方式B) が, なぜそうしたかの
  動機つきで書かれている (単なる機能列挙にしない).
- `publish.sh` でビルドが通り, RSS に著者付きで載る.

## 一次情報 (執筆時に参照する正本)

- learn repo: `~/Developer/claude/learn/` の `README.md` / `docs/spec.md` /
  `scripts/learn_{enqueue,patrol,cycle}.py?sh` / `plan/`.
- 規約: assistant `vault/agent_ops/conventions/2026-06-11-learn-recommendation-via-request-user-action.md`.
- 実 queue: existence vault `Learn/queue/2026-06-11-Learn-層の使い方.md` と
  `Learn/queue/2026-06-11-圏論入門-—-台帳の並列意味論.md`.
- 機微情報を含めない (私生活・求人・PII は記事化対象外).
