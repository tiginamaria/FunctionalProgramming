{-# LANGUAGE InstanceSigs #-}
module Sem06 where

import Prelude hiding(ShowS)
import Data.List hiding(permutations)


data Tree a = Leaf a | Branch (Tree a) a (Tree a)

-- Домашнее задание

-- Некоторое количество заданий на деревья
-- 1. Напишите инстанс Show для нашего типа бинарного дерева. 
-- Причем выводиться оно должно примерно как-то вот так:
-- GHCi> Leaf 1
-- -1
-- GHCi> Branch (Leaf 1) 2 (Leaf 3)
-- -2
--  |--3
--  |--1
-- (2 балла)
{-
 - закоменчено, что бы GHCi не ругался на два инстанса. У объявления можно убрать deriving Show 
 - и раскоментировать кастомный инстанс
instance Show a => Show (Tree a) where
    show = undefined --какой метод вам кажется более удобным, тот и реализуйте
-}

instance Show t => Show (Tree t) where
    show = myShowsTree " "
   
myShowsTree :: Show t => String -> Tree t -> String
myShowsTree s (Leaf v)         =  "-" ++ show v ++ "\n"
myShowsTree s (Branch t1 v t2) =  "-" ++ show v ++ "\n" ++ s ++ "|-" ++ (myShowsTree (s++"|  ") t1) ++ s ++ "|-" ++ (myShowsTree (s++"   ") t2)

-- 2. Изучите инстанс Read для нашего типа списков сверху. Напишите инстанс Read для типа бинарных деревьев
-- Так, что бы они парсились из строк вида:
-- GHCi> reads "<1 {2} 3>" :: [(Tree Int, String)]
-- [(Branch (Leaf 1) 2 (Leaf 3), "")]
-- (1 балл)


myReadsTree :: Read a => ReadS (Tree a)
myReadsTree ('<':s) = [(Branch t1 v t2, r) | (t1, ' ':'{':s') <- myReadsTree s, (v, '}':' ':s'') <- reads s', (t2, '>':r) <- myReadsTree s'']
myReadsTree s       = [(Leaf x, s') | (x, s') <- reads s] 

instance Read a => Read (Tree a) where
   readsPrec _ = myReadsTree 


 -- 3. Пусть у нас есть тип арифметических выражений
 
data Expr = Lit Int           -- целочисленные литералы
            | BinOp BinOp Expr Expr -- бинарные операции
            | UnOp UnOp Expr       -- унарные операции

data BinOp = Add | Mul | Sub | Div

data UnOp = Minus

-- Напишите для него инстанс Show так, что бы выражение печаталось в строку с обратной польской нотацией
-- (2 балла)

instance Show Expr where
    show = myShowsExpr
    
myShowsExpr :: Expr -> String

myShowsExpr (Lit l)           = show l
myShowsExpr (UnOp uop e)      = myShowsExpr e ++ " " ++ "~" ++ " " 
myShowsExpr (BinOp bop e1 e2) = myShowsExpr e1 ++ myShowsExpr e2 ++ " " ++ showBinOp bop ++ " " where
                                  showBinOp Add = "+"
                                  showBinOp Mul = "*"
                                  showBinOp Sub = "-"
                                  showBinOp Div = "/"


-- 4. Изучите тайпкласс Ord и как устроена его реализация по умолчанию
-- Пусть у нас есть тип данных для уровней логгирования

data LogLevel = Error | Warning | Debug | Info deriving (Show, Eq, Ord)

-- Реализуйте функцию, которая сравнивает два уровня логгирования так что бы имел место порядок
-- Info < Debug < Warning < Error

-- При этом запрещается в явном виде паттернматчится по уровню логгирования
-- Подсказка. Возможно, вам потребуется что-то дописать в определение LogLevel
-- (2 балла)

compareLogLevel :: LogLevel -> LogLevel -> Ordering
compareLogLevel l1 l2 | l1 < l2   = GT
                      | l1 > l2   = LT
                      | otherwise = EQ

-- 5. Реализуйте функцию, которая бы выдавала все перестановки элементов данного ей на вход списка
-- Для простоты считайте, что элементы в списке не повторяются
-- (1 балл)

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = [ y:zs | (y,ys) <- selections xs, zs <- permutations ys]

selections :: [a] -> [(a,[a])]
selections []     = []
selections (x:xs) = (x,xs):[ (y,x:ys) | (y,ys) <- selections xs]

-- 6. Напишите функцию, которая бы генерировала все сочетания по заданому числу элементов заданного списка
-- Для простоты считайте, что элементы в списке не повторяются
-- Обеспечьте разумное поведение для бесконечных списков

combs :: Int -> [a] -> [[a]]
combs 0 _        = [[]]
combs k []       = []
combs i l@(x:xs) = [ x:ys | ys <- combs (i-1) xs] ++ (combs i xs)

