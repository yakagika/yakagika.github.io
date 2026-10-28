set -e
stack build
stack exec main rebuild
uv run --no-project python scripts/check_local_refs.py docs
git add --all
git commit -m "Publish."
git push -u origin main
