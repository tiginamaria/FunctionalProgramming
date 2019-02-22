{-# LANGUAGE BangPatterns #-}
module Main where

-- Это разные примеры кода, которые вылетают с нехваткой памяти
-- Собирать: ghc main.hs -o main -O0 -with-rtsopts "-M16m"
-- Запускать: ./main

import Prelude hiding(foldr, foldl, foldl', concat)
import Data.Function
import Data.Bool

foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

foldl' f z [] = z
foldl' f z (x:xs) = (foldl' f $! f z x) xs

concat = foldr (++) []

mysum = foldr (+) 0

mysum' = foldl (+) 0

mysum'' = foldl' (+) 0

myavg = uncurry (/) . foldl' (\(acc, len) x -> (acc + x, len + 1)) (0, 0)

myavg' = uncurry (/) . foldl' (\(!acc, !len) x -> (acc + x, len + 1)) (0, 0)

fib :: Integer -> Integer
fib n = fst( snd (until ((>=n) . fst) (\(i, (f1, f2)) -> (i + 1, (f2, f1 + f2))) (0, (0, 1))))

ofDifferentParity :: Integer -> Integer -> Bool
ofDifferentParity n m = ((/=) `on` odd) n m

doubleFacFix :: Integer -> Integer
doubleFacFix = fix (\rec n -> if n <= 1 then 1 else n * rec (n-2)) 

doubleFacUntil :: Integer -> Integer
doubleFacUntil n = snd (until ((<=1) . fst) (\(i, acc) -> (i - 2, i * acc)) (n, 1))

nat :: (Integer -> a) -> (a -> b -> b) -> b -> Integer -> b
nat f g ini n = if n <= 9 then g (f n) ini else nat f g (g (f (n `mod` 10)) ini) (n `div` 10)


--main = print $ mysum [1..1000000]
--main = print $ mysum' [1..1000000]
--main = print $ mysum'' [1..1000000]
--main = print $ length $ concat [[x] | x <- [1..1000000]]
--main = print $ myavg [1..1000000]
main = print $ myavg' [1..1000000]
