#!/bin/sh
# 論文 DB から講義用の bib (bib/lectures.bib) を生成する.
# 対象は note の projects: に blog-lectures を持つ文献すべて. 検証の水準による絞り込みは
# ここではせず, scripts/check_citations.py が引用ごとに判定する (none は失敗, identity と
# abstract は未照合として一覧に出す).
# --profile csl は和文の「姓 名」を「姓, 名」にする (citeproc が後ろの語を姓として読むのを防ぐ).
# 計画: plan/in-progress/lecture-literature-grounding.md
set -eu
REPO=$(cd "$(dirname "$0")/.." && pwd)
PAPERS=${PAPERS:-$HOME/Documents/Papers}
cd "$PAPERS"
uv run _db/tools/make_bib.py --project blog-lectures --profile csl -o "$REPO/bib/lectures.bib"
echo "生成しました: bib/lectures.bib ($(grep -c '^@' "$REPO/bib/lectures.bib" || true) 件)"
