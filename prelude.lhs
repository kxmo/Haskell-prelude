This file contains a reimplementation of many of the functions in Prelude.

\begin{code}
import Prelude hiding (reverse, take, drop, zip, unzip, elem, filter, takeWhile, dropWhile)

reverse :: [a] -> [a]
reverse [] = []
reverse (item:list) = (reverse list) ++ [item]

take :: Int -> [a] -> [a]
take _ [] = []
take count list@(x:xs) | count <= 0 = []
                       | count > length list = list
                       | otherwise = x : (take (pred count) xs)

drop :: Int -> [a] -> [a]
drop _ [] = []
drop count list@(x:xs) | count <= 0 = list
                       | count > length list = []
                       | otherwise = drop (pred count) xs

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (a:as) (b:bs) = (a,b) : zip as bs

unzip :: [(a,b)] -> ([a],[b])
unzip list = (as, bs)
  where
    as = map fst list
    bs = map snd list
\end{code}
