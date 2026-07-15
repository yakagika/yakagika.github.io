---
plan_id: fp-semantics-article-and-traversable
status: proposed
created: 2026-07-14
updated: 2026-07-15
priority: medium
next_actor: user
next_action: "残件は (b) 層記事のみ: 骨子承認 + FAM 未収載要素との束ね方の決定. Traversable (旧 B) は 2026-07-15 に fp11 末尾「構造を走査する」として実装済み"
---

# 意味論 (b) 層の blog 記事 + Traversable の追加 (framing 改修からの切り出し)

## メタ情報

- **状態**: proposed (2026-07-14 に plan/landed/2026-07-14-fp-framing-hybrid.md から切り出し. ユーザ指示「A-4 と B は後で行うとして別計画に残す」)
- **由来**: handoff `desktop_app-2026-07-14T17-54-21-619585` の §A-4 と §B.

## 1. 意味論 (b) 層の blog 記事 (旧 A-4)

講義本体には入れない. 全道具を体験した後の総括として blog 記事 1 本に切り出す.

- 仮題: 「エフェクトとは何か: 構造に意味を割り当てる (fp 講義の補遺)」
- 骨子: ① 意味論の二義性 (プログラミング上の目的 / 表示的意味論としての意味付与) ② エフェクト = 解釈語 (同じ $(T, \eta, \mu)$ に「失敗の伝播」「非決定性」「外界への作用」という解釈を割り当てる設計) ③ 群作用の類比 ④ 代数的仕様と algebraic effects (名前の紹介と読書案内まで) ⑤ 数学的実在 vs 工学的解釈の二層.
- FAM 2 ソート handoff の未収載要素 (単ソート定義・カリー化随伴・閉圏・hom 同型・DerivingVia, 詳細は plan/landed/2026-07-14-fam-2sort-lecture-reflection.md) を同記事に束ねるか別記事にするかは着手時に判断を仰ぐ.

## 2. Traversable の追加 (旧 B) — ✅ 実装済み (2026-07-15)

**fp11 末尾の部「構造を走査する」として実装済み** (ユーザ AUQ で案 a = fp11 末尾を選択. traverse/sequenceA/一括検証/traverseTree/State での labelTree/Traversable クラスと Foldable 対比/なぜ Applicative で十分か/Exercise CH11-5. 詳細は plan/in-progress/fp-v2-unified-rebuild.md ロードマップ A5 項). 以下は当時の検討記録.

- 位置づけ = **Applicative の「需要側の回収」**. `traverse :: Applicative f => (a -> f b) -> t a -> f (t b)` は「独立エフェクトを走査して融合する」Applicative の実際の使い所.
- 「なぜ Monad でなく Applicative で十分か」(走査の形が入力コンテナで静的に決まり, 途中の値に依存しない) が Applicative の静的性・独立性の最良の実例.
- `sequenceA` / `traverse` を「構造をなぞって作用を集める」操作として提示. Foldable との対比を一節. State との traverse (静的な形 + 動的な状態, ラベル付け) を発展例に.
- 章立て (fp11 の後ろか独立節か) は Phase A5 = fp11 執筆計画 (plan/in-progress/fp-v2-unified-rebuild.md) と統合して構成案を提示する.

## 依存・関連

- plan/landed/2026-07-14-fp-framing-hybrid.md (A-1〜A-3 は 2026-07-14 実装済み)
- plan/in-progress/fp-v2-unified-rebuild.md (Phase A5: fp11)
- plan/landed/2026-07-14-fam-2sort-lecture-reflection.md
