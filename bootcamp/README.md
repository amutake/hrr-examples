BootCamp
========

```sh
# cd
% cd rr-examples/bootcamp

# hsenv の作成
% hsenv
% source .hsenv/bin/activate

# haskell-relational-record 関連のパッケージをインストール (Hackage にはまだ上がっていないので clone してから)
% git clone git@github.com:khibino/haskell-relational-record.git
% cd haskell-relational-record
% make install
% cd ../

# テーブルを作る (PostgreSQL)
% createdb testdb
% psql -f create.sql testdb

# テーブルに初期データを入れる
% runghc init.hs # or runghc random-init.hs

# 実行
% runghc main1.hs
% runghc main2.hs
```
