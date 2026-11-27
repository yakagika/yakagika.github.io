set -e
# 生成物 (docs/ と未照合の引用の一覧) 以外に未 commit の変更があれば止める.
# 公開の commit に並行 session の作業中の file を入れないため. 本文を直したら先に commit してから公開する.
other=$(git status --porcelain --untracked-files=all | grep -v -E '^.. (docs/|writing/citations/unverified\.md$)' || true)
if [ -n "$other" ]; then
  echo "生成物以外に未 commit の変更があります. 先に commit するか退避してから公開してください:" >&2
  echo "$other" >&2
  exit 1
fi
# 講義資料の引用を bib と論文 DB の検証の水準に突き合わせる (失敗があれば止める)
uv run --no-project python scripts/check_citations.py
# 変更した講義資料のページに note と warn の囲みが無ければ警告する (止めない)
uv run --no-project python scripts/check_callouts.py
stack build
stack exec main rebuild
uv run --no-project python scripts/check_local_refs.py docs
git add --all -- docs writing/citations/unverified.md
git commit -m "Publish."
git push -u origin main
