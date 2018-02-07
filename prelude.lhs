This file contains a reimplementation of many of the functions in Prelude.

\begin{code}
import Prelude hiding (reverse, take, drop, zip, unzip, elem, filter, takeWhile, dropWhile, map)

reverse :: [a] -> [a]
reverse [] = []
reverse (item:list) = (reverse list) ++ [item]

take :: Int -> [a] -> [a]
take _ [] = []
take count list@(x:xs) | count <= 0 = []
                       | count > length list = list
                       | otherwise = x : (take (pred count) xs)
\end{code}
