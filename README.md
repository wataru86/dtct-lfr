# インストール方法

```bash
$ git clone https://github.com/wataru86/dtct-lfr.git
$ cd dtct-lfr
$ stack install
$ stack exec -- hoogle-reuse generate
```

# 実行方法

```bash
$ stack exec -- dtct-lfr <Hoogle検索結果の有効件数> <検出対象ファイルのパス>
```
