---
title: "API によるデータの取得"
kind: theory
open: true
---

# API とは {#api-basics}

外部サービスが公開するデータを, API を通じてプログラムから取得できます. API の仕組みを知っている場合は, [API キーの管理](#api-key-management)へ進んでください.

::: note
**API (Application Programming Interface)** は, ソフトウェア同士がデータや機能をやりとりするための窓口です. 開発者は API を通じて他のサービスやアプリケーションの機能を利用できるため, 複雑な処理を簡単に実行したり, 他のサービスと連携したりできます.

API の利用に関わる主な通信技術には, HTTP/HTTPS, REST (Representational State Transfer), SOAP (Simple Object Access Protocol), WebSocket などがあり, それぞれ異なる目的や特徴を持っています.
:::

[EDINET API](block:api-edinet) と [X API](block:api-x) は, どちらも `REST` というアーキテクチャで提供されています. その概要は以下のとおりです.

::: note
- `HTTP/HTTPSプロトコル`

APIの通信は通常,ウェブブラウザと同様にHTTPやHTTPSプロトコルを使って行われます.HTTPSは通信を暗号化し,セキュリティを確保するため,多くのAPIで推奨されます.

- `REST（Representational State Transfer）`

RESTは,HTTPを利用してリソース（データや機能）にアクセスするための設計原則で,最も広く使われるAPI通信の形式です.REST APIでは,`GET`,`POST`,`PUT`,`DELETE`などのHTTPメソッドを使ってデータを取得,作成,更新,削除します.シンプルかつ軽量なため,モバイルアプリやWebサービスでの利用に適しています.

- `GET`

概要: リソース（データ）の取得に使用される.

例: ユーザー情報を取得する場合,GET /users/123のように送信すると,ユーザーIDが123のデータが返されます.

特徴: サーバー上のデータを変更しない「読み取り専用」操作.

- `POST`

概要: 新しいリソースを作成するために使用される.

例: 新しい記事を投稿する場合,POST /articlesで記事データをサーバーに送信すると,新しい記事が作成されます.

特徴: サーバーにデータを送信して新しいエントリを追加する操作.

- `PUT`

概要: 既存のリソースを更新するために使用される.

例: 記事の内容を変更する場合,PUT /articles/456で新しいデータを送信し,記事ID456の内容を更新します.

特徴: 指定されたリソース全体を置き換える操作.

- `DELETE`

概要: リソースの削除に使用される.

例: 記事を削除する場合,DELETE /articles/456を実行すると,記事ID456が削除されます.

特徴: サーバーからリソースを取り除く操作.

:::

REST API でデータをやりとりする際は, **JSON (JavaScript Object Notation)** が一般的に使われます. JSON はシンプルで軽量なテキスト形式で, 読みやすく, プログラミング言語間の互換性も高いため, 多くの API が標準の形式として採用しています ([Python の共通資料の JSON の節](/lectures/python5.html#json)も参照).

~~~ json
{
    "id": 123,
    "name": "John Doe",
    "email": "johndoe@example.com"
}
~~~

EDINET API と X API からデータを取得する例では, `GET` メソッドだけを使います.

::: {.warn #api-key-management}
**API キーの管理**

**API キー**は, API にアクセスする際に送る認証情報です. パスワードと同じ扱いをしてください. ソースコードに直接書くとファイルの提出・共有・公開時に漏洩するので, キーはコードと分けて保管してください.
:::
