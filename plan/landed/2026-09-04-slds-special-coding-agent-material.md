---
plan_id: slds-special-coding-agent-material
status: landed
created: 2026-06-21
updated: 2026-07-14
priority: medium
next_actor: none
next_action: "なし (common-agent-literacy-material.md へ吸収)"
---

# 特別講義DS: 学生向けコーディングエージェントの使い方 (教材)

## メタ情報

- **状態**: landed (2026-09-04 に `plan/in-progress/common-agent-literacy-material.md` へ吸収)
- **由来**: handoff `python-todoist-2026-06-21T16-04-50-360465` (Todoist 6grpJHXHmGhhPrpX, 特別講義DS project)
- **class**: substantive → ユーザ承認待ち

## 内容

「学生向けコーディングエージェントの使い方」の教材コンテンツを blog repo で作成/整備する (特別講義DS 向け).

## 関連

- `plan/proposed/free-cli-agent-for-univ-course.md` (無料 CLI Agent 環境の選定ブリーフ) — 環境面の素材として直結.
- `plan/proposed/teaching-reification-cognitive-skills.md` — 教育設計面で関連の可能性.

## 未確定事項 (承認時に確認)

- 配置先: `lectures/slds/` の補足章か, 特別講義用の独立ページ/チェーンか.
- 対象エージェント (無料枠前提か, 大学契約前提か — free-cli-agent ブリーフの結論に依存).

## 吸収 (2026-09-04)

本提案の内容は `plan/in-progress/common-agent-literacy-material.md` が引き継いだ.

- 配置先は `lectures/common/agent.md` (講義横断の共通資料). slds の補足章ではない.
- 対象エージェントは **codex** (OpenAI Codex CLI), ターミナルは **herdr**.
  学生は ChatGPT Plus $20/月 を 3 ヶ月契約する.
  `free-cli-agent-for-univ-course.md` の 2026-06 時点の結論 (Gemini 無料キー + Aider / Cline)
  は差し替わった.
