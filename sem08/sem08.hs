module Sem08 where

import Control.Applicative
import Data.Bool

-- Функторы. Обычные(немного) и аппликативные

-- Я мог бы снова сорваться на теоркат, но не буду этого делать(постараюсь)
-- В С++(по модулю того, что я на нем давно не писал) функтор -- это объект-функция - жалкая попытка дать функциям гражданство. В хаскеле все не так
-- О функторе удобно думать как о типе, который можно отобразить.
-- Например если у нас есть список чисел и функция, переводящая число в строку,
-- то благодаря структуре функтора мы можем получать списки,
-- в которых лежат строковые представления чисел. Я утрирую, но общая идея такова

-- Определение класса типов Functor:

class MyFunctor f where
    myfmap :: (a -> b) -> f a -> f b

-- У функторов есть два закона, которые растут из теоретико-категориального определения
-- и суть которых состоит в том, что при отображении нашего типа его структура не должна меняться.

-- Убедитесь, что законы функтора выполняются для списков.
-- Формально, докажите, что для любого __конечного__ списка xs и подходящих функций f и g верно:
-- 1. myfmap id = id
-- 2. myfmap (f . g) = myfmap f . myfmap g

-- Почему конечного? Почему мы не можем доказать их для бесконечных списков?

-- В отличии от обычных, аппликативные функторы позволяют вам не просто применять чистую функцию к значению в контейнере, но и "вкладывать" её в контейнер. У аппликативных функторов так же есть законы, которым должен подчиняться каждый представитель:

-- 1. pure id <*> v = v
-- 2. pure f <*> pure x = pure (f x)
-- 3. pure (.) <*> f <*> g <*> x = f <*> (g <*> x)
-- 4. f <*> pure x = pure ($ x) <*> f

-- Определение класса типов Applicative:

class MyFunctor f => MyApplicative f where
    mypure :: a -> f a
    myapp :: f (a -> b) -> f a -> f b

-- Вообще в Applicative myapp пишется как <*> и произносится как apply

-- В качестве разминки:
-- Выразите с помощью myapp метод <*
-- Его тип похож на const, но, в отличии от него, он позволяет "поработать"
-- аппликативу, при этом забив на второй аргумент
-- Например:
-- [1,2,3] `const` [3,4,5] = [1,2,3]
-- [1,2,3] <* [3,4,5] = [1,1,1,2,2,2,3,3,3]
-- Произносится как left apply

instance MyFunctor [] where
    myfmap = map

instance MyApplicative [] where
    mypure x = [x]
    fs `myapp` xs = [f x | f <- fs, x <- xs]


infixl 4 <*

(<*) :: MyApplicative f => f a -> f b -> f a
fa <* fb = mypure const `myapp` fa `myapp` fb 

-- В качестве разминки. Чему равны значения выражений:
-- [(*0), (+100), (^2)] <*> [1,2,3]
-- (+) <$> [1,2] <*> [3,4]
-- getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"

-- Как мы узнали на лекции, тип функций с фиксированным доменом -- это функтор.
-- где fmap -- это всего-навсего композиция.
-- Напишем для него представителя Applicative

instance MyFunctor ((->) e) where
    myfmap f g = \x -> f (g x) -- myfmap = (.) если в бесточечном стиле


instance MyApplicative ((->) e) where
    mypure = const
    ff `myapp` fx = \x -> ff x (fx x)

-- Выпишем тип <*>:
-- (e -> (a -> b)) -> (e -> a) -> (e -> b)
-- Или же(если убрать лишние скобки):
-- (e -> a -> b) -> (e -> a) -> e -> b
-- Ничего не напоминает?

-- Пусть мы все еще рассматриваем наш тип-стрелку. Какой тип имеют f, g, h?
-- 1. \f g h -> f <*> g <*> h
-- f :: e -> (a -> b -> c)
-- g :: e -> a
-- h :: e -> b
-- f <*> g <*> h :: e -> c
-- 2. \f g h -> f <$> g <*> h
-- f :: a -> b -> c
-- -//-

-- Как можно переписать эти конструкции без аппликатива?
-- Хинт -- не обязательно даже лезть в определение,
-- достаточно просто решить уравнения на типы

-- t1 = \f g h x -> f x (g x) (h x)
-- t2 = \f g h x -> f (g x) (h x)

-- Еще несколько мозголомательных примеров.
-- Чему равны значения следующих выражений:


-- (pure 3) "blah"
-- (+) <*> (*3) $ 4
-- zip <*> tail $ [1..10]
-- (+) <$> (+2) <*> (*3) $ 10


-- Пример Applicative(наконец-то что-то написать!) для копроизведения

instance MyFunctor (Either e) where
    myfmap _ (Left e) = Left e
    myfmap f (Right x) = Right $ f x

instance MyApplicative (Either e) where
    mypure = Right
    Right f `myapp` x = f `myfmap` x
    Left e1 `myapp` _ = Left e1


-- Напишите функцию, которая делает следующее:
-- Берет список значений, лежащих в некотором аппликативном контексте
-- Пересобирает этот список так, что значения вынимаются из контекста,
-- а список вкладывается в него

sequence' :: Applicative f => [f a] -> f [a]
sequence' [] = pure []
sequence' (fx : fxs) = 
    let 
        rec = sequence' fxs
    in (:) <$> fx <*> rec

-- Напишите аппликативный аналог функции replicate.
-- Она должна повторять некоторый аппликативный эффект заданое число раз

replicateA' :: Applicative f => Int -> f a -> f [a]
replicateA' = (sequence' .) . replicate 

-- Наконец, напишите функцию, которая фильтрует список с помощью
-- заданного предиката, но который еще и порождает эффект

filterA :: Applicative f => (a -> f Bool) -> [a] -> f [a]
filterA p = foldr (\x fxs -> (\b -> if b then (x:) else id) <$> p x <*> fxs) $ pure [] 

-- Parsec
-- Я не успел показать эту штуку на паре, но вот есть известная библиотека парсер-комбинаторов Parsec. Идея в том, что сложные парсеры можно комбинировать из более простых, используя её аппликативный интерфейс.

-- поставить(на всякий случай, она вроде идет из коробки с платформой):
-- $ cabal update
-- $ cabal install parsec

-- GHCi> import Control.Applicative((<*>), (<*), (*>), (<$>))
-- GHCi> import Text.Parsec
-- GHCi> :set -XFlexibleContexts

-- Функция parse применяет парсер rule ко входной строке text.
-- Вторым параметром она принимает имя файла с текстом, на которое мы забъем
-- В результате у нас получается либо результат парсинга, либо сообщение
-- об ошибке с указанием номера строки и символа в ней и более подробным текстом
-- Например:

-- GHCi> let prs rule text = parse rule "" text
-- GHCi> prs digit "123ABC"
-- Right '1'
-- GHCi> prs letter "123ABC"
-- Left (line 1, column 1):
-- unexpected "1"
-- expecting letter

-- digit и letter это парсеры, которые парсят по одной цифре и букве
-- соответственно. Есть парсеры, которые парсят конкретные строки(string)
-- или символы(char). Например:

-- GHCi> prs (string "ABC") "ABC123"
-- Right "ABC"
-- GHCi> prs (char 'A') "ABC123"
-- Right 'A'
-- GHCi> prs (string "ABC") "123ABC"
-- Left (line 1, column 1):
-- unexpected "1"
-- expecting "ABC"
-- GHCi> prs (char 'A') "123ABC"
-- Left (line 1, column 1):
-- unexpected "1"
-- expecting "A"

-- Если мы захотим распарсить не одну цифру, а число, то мы можем модифицировать
-- данный нам парсер для цифр, так что бы он много раз применялся ко входной
-- строке. Для этого нам помогут комбинаторы many(ноль или более раз)
-- или many1(один или более раз). Например:

-- GHCi> prs (many digit) "123ABC"
-- Right "123"
-- GHCi> prs (many digit) "ABC"
-- Right ""
-- GHCi> prs (many1 digit) "123ABC"
-- Right "123"
-- GHCi> prs (many1 digit) "ABC"
-- Left (line 1, column 1):
-- unexpected "A"
-- expecting digit

-- Парсеры можно комбинировать между собой. Например мы хотим распарсить пару
-- из строчки и числа:
-- GHCi> let pairp = (,) <$> (many1 digit) <*> (many1 letter)
-- GHCi> prs pairp "123ABC"
-- Right ("123","ABC")
-- GHCi> prs pairp "ABC123"
-- Left (line 1, column 1):
-- unexpected "A"
-- expecting digit

-- pairp -- это парсер, который сначала парсит один или более раз цифру,
-- употребляя входную строку, затем парсит один или более раз букву, и затем
-- складывает результат в пару. Еще примеры:
-- GHCi> let p0 =  many1 digit <* many1 letter
-- GHCi> prs p0 "123ABC"
-- Right "123"
-- GHCi> let p1 =  many1 digit *> many1 letter
-- GHCi> prs p1 "123ABC"
-- Right "ABC"

-- Когда мы парсим строчку парсером p0, то мы сначала парсим ее левым парсером,
-- запоминаем результат, парсим остаток строки правым парсером, забиваем на его результат
-- и выдаем то, что запомнили ранее. Для p1 -- ровно наоборот, попарсили строку левым парсером,
-- забили на результат, попарсили правым и выдали то, что он распарсил

-- Больше документации вот здесь:
-- https://hackage.haskell.org/package/parsec-3.1.11/docs/Text-Parsec.html
--
-- Домашнее задание
--
-- 1. В модуле Data.List живут функции zipWith, zipWith3 и так далее, ну вы поняли
-- Напишите функции(операторы >$< и >*<), которые бы обобщали их для функций произвольного числа аргументов,
-- пряча упаковку и распаковку ZipList
-- Используйте ZipList'ы как аппликативные функторы
-- (1 балл)

(>$<) :: (a -> b) -> [a] -> [b]
(>$<) f xs = map f xs

(>*<) :: [a -> b] -> [a] -> [b]
(>*<) fs xs = getZipList $ ZipList fs <*> ZipList xs 

-- 2. Напишите аппликативные аналоги некоторых стандартных функций над списками. В этом задании нельзя использовать sequence, который мы реализовали на практике
-- (1 балл каждое)

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f = foldr (\x fxs -> (\b -> (:) b) <$> (f x) <*> fxs) $ pure []

-- как sequence, только забивает на результат
sequence_' :: Applicative f => [f a] -> f ()
sequence_' [] = pure ()
sequence_' (fx : fxs) =
    let 
        rec = sequence_' fxs
    in (:) <$> fx *> rec

-- применяет эффекты, но забивает на результат(не используя sequence_)
mapA_ :: Applicative f => (a -> f b) -> [a] -> f ()
mapA_ f = foldr (\x fxs -> (\b -> (:) b) <$> (f x) *> fxs) $ pure ()

-- 3. Напишите инстанс Applicative для типа бинарного дерева. 
-- Семантика <*> должна соответствовать покомпонентному применению(как у ZipList). 
-- Докажите, что ваша реализация удовлетворяет законам этого тайпкласса(при доказательстве считайте, что законы функтора выполняются)
-- (1 балл за инстанс + 3 балла за доказательство)

data Tree a = Leaf | Branch (Tree a) a (Tree a) deriving Show

instance Functor Tree where
    fmap f Leaf = Leaf 
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

instance Applicative Tree where
    pure a = let t = Branch t a t in t
    (<*>) Leaf _ = Leaf
    (<*>) _ Leaf = Leaf
    (<*>) (Branch fl fx fr) (Branch l x r) = Branch (fl <*> l) (fx x) (fr <*> r)
    
{- Все законы доказываются индукцией по структуре:

1) fmap f t = pure f <*> t

   fmap f t =
   case t of
		(Leaf) -> Leaf
		(Branch l x r) -> Branch (fmap f l) (f x) (fmap f r) 
   
   pure f = Branch (pure f) f (pure f)                                   -- в структуре pure f никогда не будет листа SO далее случай <*> Leaf _ не будет рассмотрен
     
   pure f <*> t = 
   case t of
		(Leaf) -> Leaf               
		(Branch l x r) -> Branch (pure f <*> l) (f x) (pure f <*> r)
		
   для t = Leaf верно
   для t = Branch по индукции (fmap f l) = (pure f <*> l) и (fmap f r) = (pure f <*> r)=>
   fmap f t = Branch (fmap f l) (f x) (fmap f r) = Branch (pure f <*> l) (f x) (pure f <*> r) = pure f <*> t
   
   
2) pure id <*> t = t

   pure id = Branch (pure id) id (pure id)                                -- в структуре pure id никогда не будет листа SO далее случай <*> Leaf _ не будет рассмотрен
     
   pure id <*> t = 
   case t of
		(Leaf) -> Leaf               
		(Branch l x r) -> Branch (pure id <*> l) (id x) (pure id <*> r)   -- id x = x
		
   для t = Leaf верно
   для t = Branch по индукции (pure id <*> l) = l и (pure id <*> r) = r =>
   pure id <*> t = Branch (pure id <*> l) (id x) (pure id <*> r) = Branch l x r = t
   
   
3) pure g <*> pure x = pure (g x)

   pure (g x) = Branch (pure g x) (g x) (pure g x)
   pure g = Branch (pure g) g (pure g)                                   -- в структуре pure g никогда не будет листа SO далее случай <*> Leaf _ не будет рассмотрен
   pure x = Branch (pure x) x (pure x)                                   -- в структуре pure x никогда не будет листа SO далее случай <*> _ Leaf не будет рассмотрен
   
   pure g <*> pure x = Branch (pure g <*> pure x) (g x) (pure g <*> pure x)

   По индукции (pure g <*> pure x) = pure (g x) для поддеревьев =>
   pure g <*> pure x = Branch pure (g x) (id x) pure (g x) = pure (g x)
   
   
4) t <*> pure x = pure ($ x) <*> t
   
   pure x = Branch (pure x) x (pure x)                                -- в структуре pure id никогда не будет листа SO далее случай <*> Leaf _ не будет рассмотрен
   pure ($ x) = Branch (pure ($ x)) ($ x) (pure ($ x))
    
   t <*> pure x = 
   case t of
		(Leaf) -> Leaf               
		(Branch fl fx fr) -> Branch (fl <*> pure x) (fx x) (fr <*> pure x)
   
   pure ($ x) <*> t = 
   case t of
		(Leaf) -> Leaf               
		(Branch fl fx fr) -> Branch (pure ($ x) <*> fl) (($ x) fx) (pure ($ x) <*> fr)   	
		
		
   для t = Leaf верно
   для t = Branch по индукции (pure ($ x) <*> fl) = (fl <*> pure x) и (pure ($ x) <*> fr) = (fr <*> pure x) и (fx x) = (($ x) fx) по определению $ (infixr 0 $) =>
   t <*> pure x = Branch (fl <*> pure x) (fx x) (fr <*> pure x) = Branch (pure ($ x) <*> fl) (($ x) fx) (pure ($ x) <*> fr) = pure ($ x) <*> t
   
5) pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

   pure (.) = Branch (pure (.)) (.) (pure (.))                            -- в структуре pure id никогда не будет листа SO далее случай <*> Leaf _ не будет рассмотрен
    
   Если u = Leaf или v = Leaf или w = Leaf, то очевидно и u <*> (v <*> w) = Leaf = pure (.) <*> u <*> v <*> w
   Рассмотрим только случай
   u = (Branch ul ux ur)
   v = (Branch vl vx vr)
   w = (Branch wl wx wr)
   pure (.) <*> u <*> v <*> w =
   = (Branch (pure (.) <*> ul) ((.) ux) (pure (.) <*> ur)) <*> v <*> w = 
   = (Branch (pure (.) <*> ul <*> vl) ((.) ux vr) (pure (.) <*> ur <*> vr)) <*> w =
   = Branch (pure (.) <*> ul <*> vl <*> wl) ((.) ux vx wx) (pure (.) <*> ur <*> vr <*> wr)
   
   v <*> w = 
   = Branch (vl <*> wl) (vx wx) (vr <*> wr)
   
   u <*> (v <*> w) =
   = Branch (ul <*> (vl <*> wl)) (ux (vx wx)) (ur <*> (vr <*> wr)
   
   (.) ux vx wx = ux . vx $ wx = ux ( vx (wx )) = ux (vx wx)
    
   По индукции (pure (.) <*> ul <*> vl <*> wl) = (ul <*> (vl <*> wl)) и (pure (.) <*> ur <*> vr <*> wr) = (ur <*> (vr <*> wr) и (.) ux vx wx = ux (vx wx) =>
   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
   
-}

-- 4. Мы знаем что тип функций с фиксированным доменом является функтором(и аппликативным функтором тоже).
-- Про него удобно думать, как о возможность прочитать что-то из окружения.
-- Соответственно, благодаря структуре функтора окружение можно отобразить
-- Благодря структуре аппликативного функтора, в окружении может лежать функция
-- Используя этот факт, сделайте представителями Functor и Applicative следующие типы:
-- (1 балл каждое)

newtype Arrow2 d1 d2 r = Arrow2 { getArrow2 :: d1 -> d2 -> r }
newtype Arrow3 d1 d2 d3 r = Arrow3 { getArrow3 :: d1 -> d2 -> d3 -> r }

instance Functor (Arrow2 d1 d2) where
    fmap f (Arrow2 a) = Arrow2 $ (\x y -> f $ a x y)

instance Applicative (Arrow2 d1 d2) where
    pure a = Arrow2 $ (\x y -> a)
    (<*>) (Arrow2 fa) (Arrow2 a) = Arrow2 $ (\x y -> fa x y $ a x y)
    
instance Functor (Arrow3 d1 d2 d3) where
    fmap f (Arrow3 a) = Arrow3 $ (\x y z -> f $ a x y z)

instance Applicative (Arrow3 d1 d2 d3) where
    pure a = Arrow3 $ (\x y z -> a)
    (<*>) (Arrow3 fa) (Arrow3 a) = Arrow3 $ (\x y z -> fa x y z $ a x y z)

-- Семантика должна соответствовать семантике окружения, в котором лежит два или три значения соответственно

-- 5. Используя список как аппликативный функтор с семантикой недетерминированного вычисления напишите функцию
-- которая возвращает список всех подмножеств данного на вход списка
-- Для простоты считайте, что элементы в списке не повторяются
-- (2 балла.)

powerset :: [a] -> [[a]]
powerset = foldr (\x xs -> [id, ((:) x)] <*> xs) [[]] 
