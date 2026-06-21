---
plan_id: free-cli-agent-for-univ-course
status: proposed
created: 2026-06-03
updated: 2026-06-21
priority: low
next_actor: agent
next_action: "記事化ブリーフ — blog agent が posts/2026-06-XX-free-cli-agent-for-univ-course.md として記事化"
---

# 記事化ブリーフ: 2026年 大学講義の無料 CLI AI Agent 環境をどう用意するか

## メタ情報

- **状態**: proposed (記事化ブリーフ. blog agent が posts/ に記事化する素材)
- **作成日**: 2026-06-03
- **最終更新**: 2026-06-03
- **想定 posts/ ファイル名**: `posts/2026-06-XX-free-cli-agent-for-univ-course.md`
- **由来**: chore リポの調査セッション (大学学部生向け講義で AI Agent 活用手法を扱う際の, 無料 CLI Agent 環境の選定) を orchestrator 経由で blog に引き継いだもの.

## 概要

学部生向け講義で「AI Agent 活用手法」を扱うにあたり, 予算がボトルネックになる.
当初は「大学が配布している MS365 サブスク」+「GitHub Copilot」で無料運用できると考えたが,
2026年6月時点の調査で前提が2つ崩れた:

1. **MS365 と GitHub Copilot は別製品** (MS365 Copilot != GitHub Copilot). MS365 サブスクは GitHub Copilot を含まない.
2. **GitHub Copilot の学生無料プランは 2026/4/20 に新規受付停止** (再開未定). 既存有効化済みの学生のみ継続.

加えて **Gemini CLI も 2026/6/18 に無料提供終了** (後継 Antigravity CLI は無料枠が厳しい) という,
2026年前半の「無料 CLI Agent 受難」が重なった. 最終的に, 受講 10人以下 / GPU 非搭載の Windows ラップトップ混在,
という現実的条件のもとで「クラウド BYOK + Gemini API 無料キー」で実質 0円構成に着地した, という顛末.

## 動機 (なぜ記事化するか)

- 2026年前半に無料 CLI Agent の提供条件が立て続けに変わり, 「学生に無料で配る」前提の教育者が同じ落とし穴にはまりやすい.
- 「MS365 があるから Copilot 無料」という誤解は名前の類似で広く起きうる. 明示的に潰す価値がある.
- 既存記事 (codex で論文図, Multi-Repo Orchestrator) と同じ「AI Agent を実運用に組み込む」シリーズに連なる.

## 主要ファクト (本文素材, 2026/6 時点)

### 1. MS365 と GitHub Copilot は別物

- MS365 Copilot / Copilot Chat = Word/Excel/PowerPoint/Teams の業務生産性 AI. ターミナルのコーディング Agent ではない.
- 大学の MS365 (A1/A3/A5) で無料になるのは Copilot Chat であり, GitHub Copilot は別契約/別課金. MS365 ライセンスとは紐づかない.

### 2. GitHub Copilot の現状

- **学生無料 (Copilot Pro 相当)**: 2026/4/20 に新規受付停止 (再開未定). 既存有効化済みのみ継続. 一部上位モデル (GPT-5系/Opus/Sonnet) は学生プランから除外.
- **教員無料**: GitHub Education 経由で Copilot Pro 無料が継続. 教員のデモ/教材作成には使える.
- **Copilot Free (一般無料枠)**: コード補完 2,000回/月 + チャット 50回/月. CLI/agent モードは技術的には使えるが限られた AI クレジットを消費し, Agent を数ターン回すと枯渇. Agent 中心の講義には不足.

### 3. Gemini CLI -> Antigravity CLI

- Gemini CLI は 2026/6/18 に無料 / Google AI Pro / Ultra 向け提供を終了 (Google I/O 2026/5/19 告知, 30日移行).
- 後継は Antigravity CLI (クローズドソースの `agy` バイナリ). 無料枠は Pro で月1,000クレジット, 週次上限が4-5ターンで枯渇との報告. 無料の定番としては当てにしにくい.

### 4. 代替手段の比較軸

| 手段 | 無料度 | CLI Agent | 全員配布 | 注意点 |
|---|---|---|---|---|
| GitHub Copilot Free | クレジット制 | 数ターンで枯渇 | △ | Agent 中心演習に弱い |
| GitHub Copilot 学生 | 対象なら◎ | ◎ | ❌ 新規停止 | 既存のみ |
| Gemini CLI | ◎ | ◎ | ◎ | 2026/6/18 終了 |
| Antigravity CLI | 週次上限厳しい | ◎ | △ | クローズド化 |
| ローカル LLM (Ollama+OpenCode/Aider) | ◎完全無料 | ◎ | ◎ | GPU/メモリ要, CPU のみだと Agent ループが遅い |
| BYOK (Aider/Cline/Codex CLI + APIキー) | 従量/無料キー | ◎ | △ | キー管理の手間 |

### 5. Gemini API 無料枠 (BYOK の決め手)

- Google AI Studio で無料 API キー発行 (クレカ不要/期限なし).
- Flash 系で約 1,500 リクエスト/日, 15 RPM. 各自キーなので取り合いゼロ.
- Gemini 2.5 Pro も無料だが 50 リクエスト/日と薄い -> 普段 Flash, ここぞで Pro.
- CLI 製品 (Gemini CLI) の終了とは別に, AI Studio の API 無料枠は継続している点がポイント.

### 6. 最終的な着地 (受講10人以下/GPU非搭載 Windows 混在)

- この規模ではローカル LLM は不利 (CPU 推論で Agent ループが遅い, 動く/動かないの個体差でサポート地獄).
- クラウド BYOK が運用ラク x 品質安定 x 実質0円. 予算を取るとしても従量 API で学期数千円規模, サブスクは過剰.
- 推奨スタック: Gemini 無料キー + **Aider** (CLI, 変更を diff 表示で教育向き) / **Cline** (VS Code 拡張, ターミナル苦手な学生の保険). 品質を見せる回だけ上位モデル少額.

## 構成案 (posts/ 用見出し案)

1. きっかけ — 講義で AI Agent 活用を教えたい, でも予算がない
2. 最初の誤解: 「MS365 があるから GitHub Copilot 無料」は成り立たない
3. 2026年前半の無料 CLI Agent 受難
   - GitHub Copilot 学生無料の新規停止 (4/20)
   - Gemini CLI 終了 -> Antigravity (6/18)
4. 選択肢を並べる (比較表)
5. 規模で答えが変わる: 受講10人以下 / GPU なし Windows という現実
6. 着地: Gemini API 無料キー + Aider/Cline で実質0円
7. 学生配布用セットアップの要点 (Windows 11, AI Studio キー -> 環境変数 -> aider/Cline)
8. まとめ: 「無料の定番」は半年で変わる. ツールよりも「BYOK + 無料キー」という構えで持つ

## 出典リンク

- GitHub Copilot plans (Docs): https://docs.github.com/en/copilot/get-started/plans
- 学生プラン更新告知 (community #189268): https://github.com/orgs/community/discussions/189268
- 教員無料アクセス (Docs): https://docs.github.com/en/copilot/how-tos/manage-your-account/get-free-access-to-copilot-pro
- GitHub Copilot vs Microsoft Copilot (Coursera): https://www.coursera.org/articles/github-copilot-vs-microsoft-copilot
- Microsoft 365 Copilot vs GitHub Copilot (TechTarget): https://www.techtarget.com/searchenterprisedesktop/tip/Comparing-Copilot-for-Microsoft-365-vs-GitHub-Copilot
- Gemini CLI quota & pricing: https://geminicli.com/docs/resources/quota-and-pricing/
- Gemini -> Antigravity 移行 (Google 公式): https://developers.googleblog.com/an-important-update-transitioning-gemini-cli-to-antigravity-cli/
- The Register (Antigravity 移行報道): https://www.theregister.com/ai-ml/2026/05/20/bye-bye-gemini-cli-google-nudges-devs-toward-antigravity/
- Gemini API rate limits: https://ai.google.dev/gemini-api/docs/rate-limits
- Codex CLI Windows (OpenAI): https://developers.openai.com/codex/windows
- awesome-cli-coding-agents: https://github.com/bradAGI/awesome-cli-coding-agents

## 未解決事項 / トーン上の論点

- モデル名は churn が早い (Gemini 2.5 Flash/Pro, 3.x preview 等). 記事公開時点の最新表記に合わせて確認.
- 数値 (無料枠の RPD/RPM, Copilot Free の回数) は変動するため「2026年6月時点」と明記し, 後から陳腐化しても前提が分かるようにする.
- トーン: 既存記事と同じく一人称の実体験ベース. 「半年で状況が変わるので, ツール固定でなく構えで持つ」という締めが軸.
- 学生配布手順の詳細版が chore リポ側に存在 (`ai-agent-setup-windows.md`). 記事には要点のみ転記し, 全文は載せない想定.

## 関連ファイル

- 本ブリーフ: `plans/in-progress/free-cli-agent-for-univ-course.md`
- 記事化先 (予定): `posts/2026-06-XX-free-cli-agent-for-univ-course.md`
- 学生配布手順の原本 (chore リポ): `~/Developer/claude/chore/ai-agent-setup-windows.md`

## 変更履歴

- 2026-06-03: 作成 (chore リポの調査セッションから orchestrator 経由で引き継ぎ)
