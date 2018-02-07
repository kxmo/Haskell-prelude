This file contains a reimplementation of many of the functions in Prelude.

\begin{code}
import Prelude hiding (reverse, take, drop, zip, unzip, elem, filter, takeWhile, dropWhile, map)

reverse :: [a] -> [a]
reverse [] = []
reverse (item:list) = (reverse list) ++ [item]
\end{code}
