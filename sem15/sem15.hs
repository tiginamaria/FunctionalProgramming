{-# LANGUAGE
    RankNTypes,
    MultiParamTypeClasses,
    FunctionalDependencies,
    FlexibleInstances
    #-}

module Sem15 where

-- Зипперы и оптика. У вас была лекция про зипперы -- вспомогательные структуры,
-- которые позволяют эффективно навигироваться и модифицировать некоторую другую
-- (исходную) структуру данных. Кроме того, вы посмотрели на то, как в
-- функциональном языке сделать геттеры и сеттеры первого класса

-- Для разминки посмотрим на тип данных для дерева

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Eq, Show)

-- Взяв от него производную по типовому параметру, вы поняли, как должен быть
-- устроен контекст с дыркой

data Dir = L | R deriving (Eq, Show)

data CntxT a = CntxT (Tree a) (Tree a) [(Dir, a, Tree a)] deriving (Eq, Show)

type TreeZ a = (a, CntxT a)

-- Получаем:
-- (Tree a) (Tree a) -- это два дерева ниже фокуса
-- Dir -- куда идти(налево или направо)
-- a -- значение родительского узла
-- Tree a -- второе поддерево родительского узла

-- Напишите зиппер для дерева, то есть функции для создания, навигации,
-- обратного превращения зиппера в дерево и модификации значения в фокусе
-- Для простоты считайте, что обрабатывать ошибки не нужно, деревья непустые

mktz :: Tree a -> TreeZ a
mktz (Node l a r) = (a, CntxT l r [])

left :: TreeZ a -> TreeZ a
left (a, (CntxT (Node l' a' r') r xs)) = (a', (CntxT l' r' ((L, a, r):xs)))

right :: TreeZ a -> TreeZ a
right (a, (CntxT l (Node l' a' r') xs)) = (a', (CntxT l' r' ((R, a, l):xs)))

up :: TreeZ a -> TreeZ a
up (a, (CntxT l r ((d, v, t):xs))) = case d of
                                   L -> (v, CntxT (Node l a r) t xs)
                                   R -> (v, CntxT t (Node l a r) xs)

updTZ :: a -> TreeZ a -> TreeZ a
updTZ newa (a, CntxT l r xs) = (newa, CntxT l r xs)

untz :: TreeZ a -> Tree a
untz (a, CntxT l r []) = Node l a r
untz zipper = untz $ up zipper


-- Линзы Ван Лаарховена. На лекции вы строили линзы как функцию, которая
-- для любого функтора f превращает вложение a -> f a во вложение s -> f s
-- Так и запишем

type Lens'' s a = forall f. Functor f => (a -> f a) -> (s -> f s)

-- Геттер и сеттер упаковываются следующим образом:

lens' :: (s -> a) -> (s -> a -> s) -> Lens'' s a
lens' getter setter = \ret s -> fmap (setter s) (ret $ getter s)

-- Примеры линз для пары:

_1' :: Lens'' (a, b) a
_1' = lens' fst (\(_, y) v -> (v, y))

_2' :: Lens'' (a, b) b
_2' = lens' snd (\(x, _) v -> (x, v))

-- А распаковываются с помощью нужных функторов

newtype Const a x = Const { getConst :: a }

instance Functor (Const c) where
  fmap _ (Const x) = Const x

view' :: Lens'' s a -> s -> a
view' lns s = getConst (lns Const s)

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

over' :: Lens'' s a -> (a -> a) -> s -> s
over' lns f s = runIdentity $ lns (Identity . f) s

set' :: Lens'' s a -> a -> s -> s
set' lns a = over' lns (const a)

-- Заметим, что линзы _1 и _2 удобно использовать не только для пар, но и для
-- кортежей больших размерностей. Для этого их реализуют не как свободные функции
-- а как представителей классов типов. Например

class Field1'' s a | s -> a where
  _1'' :: Lens'' s a

class Field2'' s a | s -> a  where
  _2'' :: Lens'' s a

class Field3'' s a | s -> a  where
  _3'' :: Lens'' s a

-- Вам предлагается написать представителей этих классов типов для доступа к
-- элементам внутри пар и троек
-- Подумайте, нужны ли в этих классах типов функциональные зависимости?

instance Field1'' (a, b) a where
    _1'' = lens' fst (\(_, y) v -> (v, y))

instance Field2'' (a, b) b where
    _2'' = lens' snd (\(x, _) v -> (x, v))

instance Field1'' (a, b, c) a where
    _1'' = lens' (\(a, _, _) -> a) (\(_, y, z) v -> (v, y, z))

instance Field2'' (a, b, c) b where
    _2'' = lens' (\(_, b, _) -> b) (\(x, _, z) v -> (x, v, z))

instance Field3'' (a, b, c) c where
    _3'' = lens' (\(_, _, c) -> c) (\(x, y, _) v -> (x, y, v))

-- Давайте обобщим предыдущую конструкцию. У нас были линзы, которые не
-- позволяли менять тип структуры при модификации. Более общий вариант линз:

type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

-- Именно с таким определением и работает библиотека lens
-- Предыдущее определение линз называется Lens' и определяется как

type Lens' s a = Lens s s a a

-- Тогда предыдущим определениям функций lens, view, over, set можно без
-- проблем написать более общие сигнатуры типов

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter = \ret s -> fmap (setter s) (ret $ getter s)

view :: Lens s t a b -> s -> a
view lns s = getConst (lns Const s)

over :: Lens s t a b -> (a -> b) -> s -> t
over lns f s = runIdentity $ lns (Identity . f) s

set :: Lens s t a b -> b -> s -> t
set lns a = over lns (const a)

-- Наконец, перепишите классы Field1, Field2 и Field3 что бы они подружились
-- с новым определением линз. Не забудьте про фундепсы в определениях


class Field1 s t a b | b s -> t, s -> a where
    _1 :: Lens s t a b

class Field2 s t a b | b s -> t, s -> a where
    _2 :: Lens s t a b

--class Field3 s t a b | s -> a, t -> b where
--    _3 :: Lens s t a b

instance Field1 (a, b) (c, b) a c where
    _1 = lens fst (\(_, y) v -> (v, y))

instance Field2 (a, b) (a, c) b c where
    _2 = lens snd (\(x, _) v -> (x, v))
    
    
    
    
