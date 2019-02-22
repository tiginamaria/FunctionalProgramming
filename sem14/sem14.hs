{-# LANGUAGE InstanceSigs, StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

module Sem14 where

-- Рекурсивные типы. Помните, у нас был комбинатор неподвижной точки для лямбда-термов
-- Для каждого данного ему терма он возвращал его неподвижную точку.
-- Сегодня предлагается сделать то же самое, но подняться на уровень типов
-- Для размники предлагается доказать, что следующие типы изоморфны:

-- 1. (Integer, Integer) и (Bool -> Integer)
-- 2. Either Bool Bool и (Bool, Bool)
-- 3. Integer и [()]
-- 4. Tree a и Tree' a


-- data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

data Tree' a = Empty' | Node' a (Bool -> Tree' a)

instance Show a => Show (Tree' a) where
    showsPrec _ Empty' = showString "Empty'"
    showsPrec d (Node' x f) = showParen (d > app_prec) $
        showString "Node' " .
        showsPrec (app_prec + 1) x .
        showChar ' ' .
        showsPrec (app_prec + 1) (f True) .
        showChar ' ' .
        showsPrec (app_prec + 1) (f False)
            where app_prec = 10

instance Eq a => Eq (Tree' a) where
    Empty'      == Empty'       = True
    Empty'      == x            = False
    x           == Empty'       = False
    (Node' x f) == (Node' y g)  = x == y && f True == g True && f False == g False

-- Для доказательства достаточно написать соответствующие взаимно-обратные функции

fromP :: (Integer, Integer) -> (Bool -> Integer)
fromP (x, _) True = x
fromP (_, y) False = y


toP :: (Bool -> Integer) -> (Integer, Integer)
toP f = (f True, f False)

fromE :: Either Bool Bool -> (Bool, Bool)
fromE (Left True) = (True, True)
fromE (Left False) = (True, False)
fromE (Right True) = (False, True)
fromE (Right False) = (False, False)

toE :: (Bool, Bool) -> Either Bool Bool
toE (True, x) = Left x
toE (False, x) = Right x

fromI :: Int -> [()]
fromI n 
    | n == 0 = []
    | n > 0 = replicate (2*n) ()
    | n < 0 = replicate (2 * (abs n) - 1) ()

toI :: [()] -> Int
toI [] = 0
toI l 
    | even $ length l = length l `div` 2
    | otherwise = -((length l + 1) `div` 2)
{-
fromT :: Tree a -> Tree' a
fromT Empty = Empty'
fromT (Node x l r) = Node' x (\b -> case b of True -> fromT l; False -> fromT r)

toT :: Tree' a -> Tree a
toT Empty' = Empty
toT (Node' x f) = Node x (toT $ f True) (toT $ f False)
-}
-- Оператор неподвижной точки для типов

newtype Fix f = Fix (f (Fix f))

deriving instance Show (f (Fix f)) => Show (Fix f)
deriving instance Eq (f (Fix f)) => Eq (Fix f)

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

-- Сконструируем, например, нерекурсивный тип для списка

data L a l = Nil | Cons a l deriving (Eq, Show)

-- Инстанс функтора для него

instance Functor (L a) where
    fmap :: (u -> v) -> L a u -> L a v
    fmap f Nil = Nil
    fmap f (Cons a l) = Cons a (f l)

-- Рекурсивный тип для списка -- это просто неподвижная точка L

type List a = Fix (L a)

-- Примеры списков, сконструированных таким образом

-- Fix Nil -- пустой список
-- Fix (Cons '1' (Fix Nil))
-- Fix (Cons '2' (Fix (Cons '1' (Fix Nil))))

-- Покажите, что наше определение списка полностью изоморфно обычному списку из хаскеля

from :: [a] -> List a
from [] = Fix Nil
from (x:xs) = Fix (Cons x $ from xs)

to :: List a -> [a]
to (Fix Nil) = []
to (Fix (Cons x xs)) = x : (to xs)

-- Катаморфизмы и анаморфизмы.
-- В теории категорий есть понятие F-алгебры, которое позволяет обобщить понятие алгебраической структуры
-- Это позволяет записывать законы в виде морфизмов, не думая вообще о том, что лежит внутри этой структуры
-- Хороший пример про группы написан в английской википедии - [0], например.
-- Кроме того, есть просто отличная статья от Бартоша Милевски - [1], где он раскладывает все по полочкам.
-- Если сложно читать на английском, то есть русский перевод - [3].
-- Если хочется упороться совсем, есть статья [4], в которой впервые и описаны эти концепции

-- F-алгебры образуют категорию, объекты которой -- это сами алгебры над некоторым эндофунктором F
-- а морфизмы -- так называемые гомоморфизмы F-алгебр -- это стрелки, которые сохраняют структуру
-- Нам достаточно думать о F-алгебре как о функторе f, некотором объекте-носителе(carrier) а
-- и морфизме phi :: f a -> a. Удобно думать, что функтор F -- формирует выражения в этой структуре, а
-- морфизм phi -- вычисляет их

type Algebra f a = f a -> a

-- Катаморфизм представляет собой некоторое обобщение понятия свертки.
-- Продолжая наш поход в теоретико-категориальных лесах, катаморфизм -- это уникальный
-- гомоморфизм из начальной(initial) алгебры в некоторую другую алгебру. Так и запишем:

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg (Fix x) = alg $ fmap (cata alg) x

--  Для примера рассмотрим определение нерекурсивного функтора, который кодирует натуральные числа

data N x = Z | S x deriving Show

instance Functor N where
    fmap f Z = Z
    fmap f (S x) = S $ f x

type Nat = Fix N

phiN :: Algebra N Int -- N Int -> Int
phiN Z      = 0
phiN (S n)  = succ n

toInt :: Nat -> Int
toInt = cata phiN

-- Дуальной(двойственной) конструкцией для F-алгебры является F-коалгебра

type Coalgebra f a = a -> f a

-- Они точно так же образуют категорию, с теми же объектами, и теми же гомоморфизмами в роли стрелок
-- О них можно думать, как о способе породить некоторую(возможно бесконечную) структуру.

-- Дуальным к катаморфизму является анаморфизм -- обобщение "разветки"
-- Это уникальный гомоморфизм из произвольной коалгебры в терминальную(terminal)
-- Определим его:

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg x = Fix $ fmap (ana coalg) (coalg x)

-- Например:

psiN :: Coalgebra N Int -- Int -> N Int
psiN 0 = Z
psiN n = S (n - 1)

toNat :: Int -> Nat
toNat = ana psiN

-- Наконец, есть гилеморфизм  -- последовательное применение сначала анаморфизма, а затем катаморфизма

hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo alg coalg = cata alg . ana coalg

-- Есть еще метаморфизм, когда мы сначала лепим катаморфизм, а затем анаморфизм, но я не могу привести
-- ни одного содержательного примера =(

meta :: (Functor f, Functor g) => Coalgebra f a -> Algebra g a -> Fix g -> Fix f
meta coalg alg = ana coalg . cata alg

-- На лекции вам показали некоторое количество списочных алгебр.
-- Давайте посмотрим на коалгебру для списка, которая будет вычислять нам простые числа

-- Нам понадобится нерекурсивный функтор, который описывает бесконечный список

data St e s = St e s deriving Show

instance Functor (St e) where
    fmap f (St e x) = St e (f x)

type Stream e = Fix (St e)

-- Предлагается понять, как устроена коалгебра для потока простых чисел
-- Какой тип надо ей написать?

primesCoalg :: Coalgebra (St Int) [Int] -- [Int] -> St Int [Int]
primesCoalg (x:xs) = let
        notdiv x y = y `mod` x /= 0
    in St x (filter (notdiv x) xs)


-- Ну и наконец, поток, это тот же бесконечный список поэтому предлагается написать для него алгебру
-- которая преобразует в обычный список из хаскеля

toInfListAlg :: Algebra (St e) [e] -- St e [e] -> [e]
toInfListAlg (St e xs) = e : xs

toInfList :: Stream a -> [a]
toInfList = cata toInfListAlg


-- Ссылки:
-- 0. https://en.wikipedia.org/wiki/F-algebra#Groups
-- 1. https://bartoszmilewski.com/2017/02/28/f-algebras/
-- 2. https://henrychern.files.wordpress.com/2017/10/24.pdf
-- 3. https://ris.utwente.nl/ws/portalfiles/portal/6142049


-- Домашнее задание
--
-- 1. Реализуйте нерекурсивный функторi E, который бы описывал тип арифметических выражений из прошлых дз
{-
data Expr = Lit Int
            | BinOp Expr Expr
            | UnOp Expr deriving (Eq, Show)
            
data BinOp = Add | Mul | Sub | Div | Mod deriving Show
data UnOp = Minus deriving Show
-}

data UnOp = Minus deriving Show
data BinOp = Add | Mul | Sub | Div | Mod deriving Show

data E x = Lit Int
            | BinOp BinOp x x
            | UnOp UnOp x deriving (Show)         
            
instance Functor E where
    fmap f (Lit x) = Lit x
    fmap f (BinOp op e1 e2) = BinOp op (f e1) (f e2) 
    fmap f (UnOp op e) = UnOp op (f e)

type Expr = Fix E

-- Определите тип Expr как неподвижную точку этого функтора
-- Напишите алгебру над этим функтором, которая позволяла бы задать катаморфизм, вычисляющий выражение
-- (1 балл)
eval :: Expr-> Int
eval = cata phiE
    
phiE :: Algebra E Int
phiE (Lit x) = x
phiE (UnOp op e) = negate e
phiE (BinOp op e1 e2) = case op of
                             Add -> e1 + e2
                             Mul -> e1 * e2
                             Sub -> e1 - e2
                             Div -> e1 `div` e2 
                             Mod -> e1 `mod` e2
                             


-- Заведем псевдостек и вычислитель на его основе

type Stack = [Int]

push :: Int -> Stack -> Stack
push a as = a : as

add :: Stack -> Stack
add  (a : b : cs) = (b + a) : cs

mult :: Stack -> Stack
mult (a : b : cs) = (b * a) : cs

sub :: Stack -> Stack
sub (a : b : cs) = (b - a) : cs

div' :: Stack -> Stack
div' (a : b : cs) = (b `div` a) : cs

mod' :: Stack -> Stack
mod' (a : b : cs) = (b `mod` a) : cs

minus :: Stack -> Stack
minus (c : cs) = (-c) : cs

-- Реализуйте алгебру, которая бы осуществляла вычисления
-- (1 балл)
eval' :: Expr -> Stack -> Stack
eval' = cata phiE'
    
phiE' :: Algebra E (Stack -> Stack)
phiE' (Lit x) st = push x st
phiE' (UnOp op e) st = minus $ e st
phiE' (BinOp op e1 e2) st = case op of
                             Add -> add $ e2 $ e1 st
                             Mul -> mult $ e2 $ e1 st
                             Sub -> sub $ e2 $ e1 st
                             Div -> div' $ e2 $ e1 st
                             Mod -> mod' $ e2 $ e1 st

-- 2. Возьмем тип данных для бинарного дерева

-- data Tree a = Leaf | Branch (Tree a) a (Tree a) deriving (Show, Eq)

data T a x = Leaf | Branch x a x deriving (Show)

type Tree a = Fix (T a)

instance Functor (T a) where
    fmap f Leaf = Leaf
    fmap f (Branch l a r) = Branch (f l) a (f r)
    

-- Напишите нерекурсивный функтор T, который бы описывал структру этого типа 
-- Сделайте тип данных Tree его неподвижной точкой и напишите алгебру, 
-- катаморфизм которой суммирует значения в узлах дерева
-- (1 балл)
treeSum :: Num a => Tree a -> a
treeSum = cata phiTSum
    
phiTSum :: Num a => Algebra (T a) a
phiTSum Leaf = 0
phiTSum (Branch l a r) = l + a + r


-- Напишите алгебру, катаморфизм которой сворачивал бы дерево в список значений в соответствии с 
-- in-order стратегией обхода
-- (1 балл)
treeToList :: Tree a -> [a]
treeToList = cata phiTInOrder
    
phiTInOrder :: Algebra (T a) [a]
phiTInOrder Leaf = []
phiTInOrder (Branch l a r) = l ++ [a] ++ r

-- Наконец, напишите коалгебру, анаморфизм которой порождал бы 
-- бинарное дерево поиска(меньшие элементы слева, большие справа)
-- (1 балл)
listToTree :: Ord a => [a] -> Tree a
listToTree = ana psiTBST

psiTBST :: Ord a => Coalgebra (T a) [a]
psiTBST [] = Leaf
psiTBST (x:xs) = Branch b1 x b2
    where
      b1 = filter (<= x) xs
      b2 = filter (> x) xs

-- В качестве проверки убедитесь, что гилеморфизм этих ребят -- это сортировка списка
sort :: Ord a => [a] -> [a]
sort = hylo phiTInOrder psiTBST
