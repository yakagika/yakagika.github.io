---
plan_id: seminar-page
status: in-progress
created: 2026-10-27
updated: 2026-10-27
priority: low
next_actor: user
next_action: "本人が worktree .claude/worktrees/seminar-page の pages/seminar.markdown の (記入: ...) を書く. 書き終えたら agent が main へ取り込む"
source_handoff: assistant-2026-10-26T16-20-03-271171-ゼミのページを-blog-に作る-骨組みの下書き
---

# ゼミのページの骨組み

## メタ情報

- **状態**: in-progress (2026-10-27 に本人承認)
- **作成日**: 2026-10-27
- **最終更新**: 2026-10-27
- **出所**: 本人の Todoist メモ「ゼミのページをblogに作る」(2026-10-23) を assistant が handoff にしたもの. 下書きは agent が作り, 中身は本人が書く進め方を本人が選んだ (2026-10-26).

## 概要

サイトにゼミを紹介する固定ページを 1 枚足し, 上部の navigation から辿れるようにする. 紹介文やテーマなどの中身は placeholder にして本人が書く.

## 動機

ゼミの紹介, テーマ, 進め方, 募集と連絡の方法を, 学生が 1 か所で読めるようにする.

## 設計

- 置き場所: `pages/seminar.markdown` (既存の Contact と同じ固定ページの仕組み). 出力は `/seminar.html`.
- `src/Main.hs` の固定ページの一覧 `pages` に 1 行足す.
- `templates/default.html` の navigation で Research と Contact の間に link を足す. 表示名は既存の英語の項目 (Home, Blog, Lecture, Research, Contact) に揃えて `Seminar` とする.
- 見出しの案 (本文はすべて placeholder):
  1. ゼミの紹介 (担当教員, 対象学年, 開講の形)
  2. テーマ (扱う分野と, 過去の卒業研究の題目の例)
  3. 進め方 (1 年の流れ, 毎回の進め方, 使う道具. 講義資料への link を置ける)
  4. 求める学生像と, 事前に履修しておくとよい科目
  5. 募集と連絡の方法 (説明会の日程, 連絡は大学のアドレスかグループウェアで, と Contact と同じ方針)
- 学生の氏名などの個人情報は入れない. 過去の題目の例も, 本人が載せてよいと判断したものだけを書く.
- コードとテンプレートを変えるので, 隔離した worktree で作り, build が通ることを確かめる. push しない.

## ロードマップ

1. 本人の承認 (公開の扱いと見出しの案).
2. worktree で 3 file を編集し, build を通す. プレビューで本人が確認する.
3. 本人の指示で main へ取り込む.

## コスト見積もり

agent の作業は 15 分程度. 中身の執筆は本人.

## 未解決事項 / リスク

- `publish.sh` は main を build して push するので, main へ取り込むと, 次の公開で placeholder だけのページが navigation から見える. 中身が揃うまで branch に置くか, link を外して取り込むかを本人が決める.

## 本人の裁定 (2026-10-27)

- 骨組みは branch に置き, 本人が中身を書き終えてから main へ取り込む (仮の文のページを公開しない). 見出しの案はこのまま.

## 変更履歴

- 2026-10-27: branch feat/seminar-page (fc71a57) に骨組みを作り, build が通った (`/seminar.html` が出力され, 全ページのメニューに link が入る). 本人が書く箇所は `pages/seminar.markdown` の「(記入: ...)」の 10 箇所. worktree で build するには main の CLAUDE.md の一時コピーが要る (AGENTS.md が symlink のため). 取り込み前に `docs/` を戻し, コピーを消す.
