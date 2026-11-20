set -e
# 講義資料の引用を bib と論文 DB の検証の水準に突き合わせる (失敗があれば止める)
uv run --no-project python scripts/check_citations.py
stack build
stack exec main rebuild
uv run --no-project python scripts/check_local_refs.py docs
git add --all
git commit -m "Publish."
git push -u origin main
