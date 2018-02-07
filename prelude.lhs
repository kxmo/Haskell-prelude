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
elem' element list = not . null $ filter (== element) list

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (a:as) | p a = a : rem
                | otherwise = rem
  where rem = filter p as
\end{code}

For takeWhile' we use the otherwise clause of the guard to call
our degenerate case rather than do the reversing ourselves.
This is because this centralises the 'fixing' of the list.
Should we choose in the future to add each element to the end of
the list when adding (a less efficient solution) we would need
to remove the reverse call; but only from one place. If we had
called it in two places there is an opportunity to miss one of
the calls.

\begin{code}
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p as = takeWhile' p as []
takeWhile' _ [] list = reverse list
takeWhile' p (a:as) list | p a = takeWhile' p as (a:list)
                         | otherwise = takeWhile' p [] list
\end{code}
