module Sem07 where

import Data.List(unfoldr, sortBy)

-- Домашнее задание
-- 0. Напишите некоторое количетство функций над списками с помощью сверток(foldr, foldl, foldr1, foldl1)
-- (0.1 балла каждое)

or' :: [Bool] -> Bool
or' = foldr (||) False

length' :: [a] -> Int
length' = foldr (\x y -> y + 1) 0

maximum' :: Ord a => [a] -> a
maximum' = foldr1 (max) 

head' :: [a] -> Maybe a
head' = foldr (\x y -> Just x) Nothing 

last' :: [a] -> Maybe a
last' = foldl (\x y -> Just y) Nothing

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x y -> if (p x) then x:y else y) []  

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> (f x):y) []

unzip' :: [(a, b)] -> ([a], [b])
unzip' = foldr (\x y -> ((fst x):(fst y), (snd x):(snd y))) ([], [])

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = foldr ((++) . f) []

-- постарайтесь обеспечить эффективную реализацию
reverse' :: [a] -> [a]
reverse' = foldr (\x y -> y ++ [x]) []

-- 1. Используя unfoldr, напишите следующие функции:

-- По левому и правому концу диапазона возвращает список элементов, входящих в него(т.е все x такие, что a <= x < b)
-- Обеспечьте разумное поведение для ситуации, когда a > b
-- (1 балл)
range :: Enum a => a -> a -> [a]
range a b = unfoldr fun a
   where fun = \x -> if (fromEnum x > fromEnum b) then Nothing else Just (x, succ x)

-- Повторяет заданное значение нужное число раз
-- (1 балл)
replicate' :: a -> Int -> [a]
replicate' x n = unfoldr fun n
    where fun = \n -> if n<=0 then Nothing else Just (x,n-1)

-- Циклически повторяет данный на вход список
-- (1 балл)
cycle' :: [a] -> [a]
cycle' xs = unfoldr fun xs
    where 
        fun = \x -> if null (tail x) then Just (head x, xs) else Just (head x, tail x)

-- 2. Используя foldr напишите следующие функции:
-- (1.5 балла)
drop' :: Int -> [a] -> [a]
drop' n xs = foldr fun (const []) xs n
    where 
        fun x y n | n <= 0    = x:y 0
                  | otherwise = y (n-1)

-- (1.5 балла)
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = foldr fun (const ([], [])) xs n
    where 
        fun x y n | n <= 0    = let (s1, s2) = y 0 in ([], x:s2)
                  | otherwise = let (s1, s2) = y (n-1) in (x:s1, s2)

-- Безопасный поиск по индексу(Возвращает Nothing если индекс плохой)
-- (1.5 балла)
(!!!) :: [a] -> Int -> Maybe a
xs !!! n = foldr fun (const Nothing) xs (n - 1)
    where 
        fun x y n | n == 0     = Just x
                  | n < 0      = Nothing
                  | otherwise  = y (n-1)
                    
-- (2 балла)
-- Не нужно ли здесь применить тот же трюк что и выше?
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = foldr (\x acc -> if p x then x:acc else []) []

-- 3. Не баян, а классика. Выразите левую свертку через правую
-- (1.5 балла)

foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f v xs = foldr fun ini xs v
    where
        fun = \x g a -> g (f a x)
        ini = id
        
-- 4. Не баян, а классика. У нас есть тип данных для бинарного дерева

data Tree a = Leaf | Branch (Tree a) a (Tree a) deriving Show

-- Как известно, существует некоторое количество стратегий обхода дерева. 
-- Задание. Реализуйте инстансы Foldable для каждого из них. 
-- Типы-обертки потребные для наших нужд:

newtype PreOrder a = PreOrder (Tree a) deriving Show
newtype PostOrder a = PostOrder (Tree a) deriving Show
newtype LevelOrder a = LevelOrder (Tree a) deriving Show

-- (1 балл)
instance Foldable Tree where
    foldMap f Leaf = mempty
    foldMap f (Branch l a r) = (foldMap f l) <> f a <> (foldMap f r)

-- (1 балл)
instance Foldable PreOrder where
    foldMap f (PreOrder (Leaf)) = mempty
    foldMap f (PreOrder (Branch l a r)) = f a <> (foldMap f $ PreOrder l) <> (foldMap f $ PreOrder r)

-- (1 балл)
instance Foldable PostOrder where
    foldMap f (PostOrder (Leaf)) = mempty
    foldMap f (PostOrder (Branch l a r)) = (foldMap f $ PostOrder l) <> (foldMap f $ PostOrder r) <> f a

-- (1 балл)
instance Foldable LevelOrder where
    foldMap f (LevelOrder tree) = foldMap' [tree] where
        foldMap' [] = mempty
        foldMap' ((Leaf):xs) = foldMap' xs
        foldMap' ((Branch l a r):xs)  = f a <> (foldMap' $ xs++[l]++[r])

-- 5. Пусть m -- это некоторый моноид. Тогда функции из a в m так же формируют моноид(почему?). 
-- Реализуйте инстанс моноида для них.  

-- Обертка нужна, что бы компилятор не плевался на дубликат(такой моноид уже есть)
newtype ArrowWrapper a b = ArrowWrapper { getArrow :: a -> b }

instance Semigroup m => Semigroup (ArrowWrapper a m) where
    f <> g = ArrowWrapper $ \a -> getArrow f a <> getArrow g a
    
instance Monoid m => Monoid (ArrowWrapper a m) where
    mempty  = ArrowWrapper $ \a -> mempty
    mappend = (<>)

-- Собственно задание. Пусть у нас есть знакомый вам тип данных для записей о людях

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

firstNameOrd :: Person -> Person -> Ordering
firstNameOrd p1 p2 = compare (firstName p1) (firstName p2)

lastNameOrd :: Person -> Person -> Ordering
lastNameOrd p1 p2 = compare (lastName p1) (lastName p2)

ageOrd :: Person -> Person -> Ordering
ageOrd p1 p2 = compare (age p1) (age p2)

-- Отсортируйте список людей сначала по возрасту, а затем(если у двух людей окажется одинаковый возраст) -- по фамилии. Если же вы нашли двух одногодков с одинаковыми фамилиями, то отсортируйте их по имени. Используйте sortBy для сортировки и вышеописанный моноид для еще чего-то(для чего?)
-- (1 балл)
sortPersons :: [Person] -> [Person]
sortPersons = sortBy comp 
    where comp = getArrow (ArrowWrapper ageOrd <> ArrowWrapper lastNameOrd <> ArrowWrapper firstNameOrd)
