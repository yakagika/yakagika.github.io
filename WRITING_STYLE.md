# WRITING_STYLE.md (ポインタ)

haskell-blog の日本語記事・講義の文体規範は, assistant の中央 view (profile `blog-ja`) と
本 repo の `writing/` (manifest `writing/writing-style.yaml` が指す local 規則と exceptions) にある.
`posts/`・`lectures/` の日本語散文を書く・推敲する前に skill `writing-style` を発動し,
`writing_lock.py paths` が返す view と `writing/local/*.md` を順に読んで全 rule を適用する.
repo 固有の運用は `writing/local/repo-ops.md`, 講義の内容設計の原則は `writing/local/lecture-design.md`.

旧 copy 型 (seed 2026-09-12, ported 10 件) は 2026-09-19 に参照型へ移行した.
旧版の各節の行き先は移行 commit の message にある対応表を参照する.
