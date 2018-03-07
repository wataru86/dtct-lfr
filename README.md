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

例) 
$ stack exec -- dtct-lfr 10 test/Sample.hs
Sample.hs: line 3-4: 
length' :: [a] -> Int
  <=> GHC.OldList.length :: [a] -> Int

Sample.hs: line 7-9: 
zip' :: [Char] -> [Int] -> [(Char, Int)]
  <=> Prelude.zip :: [a] -> [b] -> [(a, b)]

Sample.hs: line 12: 
map' :: (Int -> Int) -> [Int] -> [Int]
  <=> Prelude.map :: (a -> b) -> [a] -> [b]

Sample.hs: line 15: 
filter' :: Floating a => (a -> Bool) -> [a] -> [a]
  <=> Prelude.filter :: (a -> Bool) -> [a] -> [a]

Sample.hs: line 18: 
zipWith' :: (Enum b, Ord a) => (a -> a -> b) -> [a] -> [a] -> [b]
  <=> Prelude.zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

Sample.hs: line 26: 
flip' :: (Char -> Int -> [(Char, Int)]) -> Int -> Char -> [(Char, Int)]
  <=> Prelude.flip :: (a -> b -> c) -> b -> a -> c

Sample.hs: line 28: 
flip'' :: (a -> b -> c) -> b -> a -> c
  <=> Prelude.flip :: (a -> b -> c) -> b -> a -> c

```
