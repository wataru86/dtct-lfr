module Sample where

length' [] = 0
length' (_:xs) = succ $ length xs

zip' :: [Char] -> [Int] -> [( Char , Int )]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

map' :: (Int -> Int) -> [Int] -> [Int]
map' f xs = [f x | x <- xs]

filter' :: Floating a => (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, f x]

zipWith' :: (Ord a, Enum b) => (a -> a -> b) -> [a] -> [a] -> [b]
zipWith' f xs ys = map (\(x, y) -> f x y) $ zip xs ys

groupBy' :: (Char -> Char -> Bool) -> String -> [String]
groupBy' _ [] = []
groupBy' eq (x:xs) = (x:ys) : groupBy' eq zs
  where (ys ,zs) = span (eq x) xs

flip' :: (Char -> Int -> [(Char, Int)]) -> (Int -> Char -> [(Char, Int)])
flip' f x y = f y x

flip'' = flip

msg = "This is a sample program ."

main = do
  putStrLn msg
