# RSS フィードに著者情報を追加

## メタ情報

- **状態**: landed
- **作成日**: 2026-05-25
- **最終更新**: 2026-05-25
- **着地日**: 2026-05-25
- **関連タスク**: Orchestrator [tjcrghXJm] (Todoist: https://app.todoist.com/app/task/6ggMMPG9jcrghXJm)

## 概要

`docs/rss.xml` および `docs/tags/<tag>.xml` (posts/lectures タグ別 RSS) の **author 要素を補強**する. 現状は Hakyll 標準テンプレートが `<dc:creator>` (Dublin Core) しか出さず, RSS 2.0 標準の channel-level `<managingEditor>` / item-level `<author>` が欠落している. 標準的な RSS リーダや検証ツールが期待する形に揃える.

## 動機

Orchestrator から渡されたタスク 「RSS フィードに author 要素が欠落」 への対応. 既存 `feedConfiguration` に `feedAuthorName = "yakagika"`, `feedAuthorEmail = "kaya3728@gmail.com"` は定義済みだが, **email は RSS 出力に使われていない** (Hakyll 標準 RSS テンプレートが `$authorEmail$` を参照していないため). lectures タグ別フィードも同じ問題. 全 RSS フィードに channel/item 両レベルの著者情報を入れる.

## 設計

Hakyll 標準 RSS テンプレート (`rss.xml`, `rss-item.xml`) はパッケージ内で固定 — `renderRss` がそれを呼ぶ. これを上書きするには:

1. `templates/rss.xml` と `templates/rss-item.xml` をリポ側に置く
2. `renderRss` → `renderRssWithTemplates` に切替えて, ロード済みテンプレートを渡す

テンプレート変更点:

- `templates/rss.xml` (channel): `<managingEditor>$authorEmail$ ($authorName$)</managingEditor>` を追加
- `templates/rss-item.xml` (item): `<author>$authorEmail$ ($authorName$)</author>` を追加 (`<dc:creator>` は維持)

`feedAuthorEmail` は `renderRssWithTemplates` が自動で context に注入する (Hakyll の `Hakyll.Web.Feed` 実装による).

## ロードマップ

1. `templates/rss.xml` を新規作成 (Hakyll built-in をベースに author 行を追加)
2. `templates/rss-item.xml` を新規作成 (同上)
3. `src/Main.hs` の 3 箇所の `renderRss` 呼び出しを `renderRssWithTemplates` 化:
   - L204: posts タグ別 RSS
   - L277: lectures タグ別 RSS
   - L326: 全 posts RSS (`rss.xml`)
4. `stack build` でビルド, `stack exec site -- build` で生成, `docs/rss.xml` を grep で確認
5. `plans/landed/2026-05-25-rss-feed-author.md` に移動 → `complete_task(tjcrghXJm)`

## コスト見積もり

- テンプレート 2 ファイル: 各 ~15 行
- site.hs 改修: 数行 + テンプレロード行
- ビルド + 検証: 数分
- 合計: 30 分以内

## 未解決事項 / リスク

- `feedAuthorEmail` が空文字でも `<author> ()</author>` のような壊れ出力にならないか — Hakyll の context 注入挙動を要確認
  - 対応: `feedAuthorEmail` は現状 `kaya3728@gmail.com` なので問題化しない. 将来空にしたい場合は `$if(authorEmail)$ ... $endif$` で守る
- Atom フィードは作っていない (将来検討)
- lectures-only の全件 RSS (`lectures.xml`) は本タスク範囲外

## 関連ファイル

- `src/Main.hs` (`feedConfiguration`, `renderRss` 呼び出し 3 箇所)
- `templates/rss.xml` (新規)
- `templates/rss-item.xml` (新規)
- `docs/rss.xml`, `docs/tags/*.xml` (検証対象)

## 変更履歴

- 2026-05-25: 作成
- 2026-05-25: 実装完了, landed へ移動. `docs/rss.xml`, `docs/tags/*.xml` で `<managingEditor>` / `<webMaster>` / `<author>` が出力されることを確認 (xmllint OK).
