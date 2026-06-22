# files/ — Agent への一時受け渡しフォルダ

このリポジトリ専用のローカル作業用スクラッチ領域です. **system の `/tmp` とは別物**で,
ユーザが Agent に渡す資料 (PDF・CSV・スクリーンショット等) や, コードの実行・一時生成物を
ここに置きます (旧 `tmp/` から改称. `tmp` は system の `/tmp` と紛らわしく Agent が混乱したため).

`README.md` 以外の中身は `.gitignore` で commit 対象外です.
