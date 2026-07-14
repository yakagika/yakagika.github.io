---
plan_id: knowledge-projection-script
status: proposed
created: 2026-06-23
updated: 2026-07-14
priority: low
next_actor: upstream
next_action: "上流依存待ち — obsidian 統計 pilot の maturity=complete topic が出てから検証着手 (H1 完了後)"
---

# 知識層 projection script: obsidian /Knowledge/ → 簡略版 public / lecture

## メタ情報

- **状態**: proposed (handoff import, 2026-07-14 処理). **上流依存によりブロック中**
- **由来**: handoff `python-todoist-2026-06-23T12-13-11-812384`. 設計正本 = python-todoist `plan/proposed/2026-06-23-knowledge-layer-architecture.md` (D3 + projection 規則)
- **class**: substantive → ユーザ承認待ち (かつ依存待ち)

## 依存 (ブロッカー)

obsidian-vault に統計 pilot が蓄積され **maturity=complete の topic が出てから** projection を検証 (H1 完了後). 現時点では着手しない.

## 内容 (要約)

1. projection script (stdlib のみ): obsidian md frontmatter parse → maturity/level/visibility/channel フィルタ → prereq topo-sort → wiki-link→md-link 変換 → 中間 md → 既存 Hakyll/Pandoc へ.
2. 公開 projection (`maturity=complete & visibility=public`) と講義 projection (`level<=講義上限 & channels∋lecture`).
3. 生成物は blog の build/ に隔離 (personal vault と混ぜない). slds/fp の直接公開と二重運用にしない. `publish.sh` が全 source を push する点に注意 (private を source に持ち込まない).
4. Beamer/docx/tex fan-out は作らない (実需 pull で後付け).

## 受け入れ基準 (handoff 原文)

統計 pilot の maturity=complete topic から, 単一 obsidian source の projection で簡略版 public ページが 1 本生成され, level/visibility/maturity フィルタが意図通り効く. slds の直接公開と二重運用にならない. assemble が stdlib のみ.
