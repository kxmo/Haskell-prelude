This file contains a reimplementation of many of the functions in Prelude.

\begin{code}
import Prelude hiding (reverse, take, drop, zip, unzip, elem, filter, takeWhile, dropWhile)
import Data.Foldable hiding (elem)

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

elem :: (Eq a, Foldable t) => a -> t a -> Bool
elem element foldable = elem' element (toList foldable)
elem' _ [] = False
elem' element (a:rest) | a == element = True
                       | otherwise = elem element rest
\end{code}
