module Sem11 where

import Control.Monad(ap, replicateM, filterM, liftM)
import qualified Data.Set as Set
--import System.Random
import Control.Arrow
import Data.IORef

-- Стандартные монады. Reader, Writer, State и IO

-- УВАЖАЕМЫЕ ЗНАТОКИ!
-- КАК ВЫ ДУМАЕТЕ, ЧТО ДЕЛАЕТ ЭТОТ КОД:
-- f = join (*)

-- Для разминки интересно бывает посмотреть на то, насколько Хаскель
-- иногда все же ленив. Попробуйте _угадать_, что выведется на экран:
-- let x = print "A" in print "B"
-- let x = print "A" in x >> print "B"
-- (\x -> print "A") (print "B")
-- (print "A") `seq` (print "B")

-- Ну а теперь про монаду Reader. На лекции у вас было вот такое определение:

newtype Reader r a = Reader { runReader :: r -> a }

instance Monad (Reader r) where
    return      = Reader . const
    m >>= k     = Reader $ \e ->    let v = runReader m e
                                    in runReader (k v) e

-- На лекции вам показывали интерфейс сборки-разборки Reader в виде двух функций:
-- reader :: (r -> a) -> Reader r a
-- runReader :: Reader r a -> (r -> a)

-- Тут вообще просят написать reader, но очевидно, что для нашего типа это просто констурктор Reader

-- Напишите инстансы Applicative и Functor для него

instance Functor (Reader r) where
    fmap = liftM 

instance Applicative (Reader r) where
    pure = return
    (<*>) = ap

-- Наконец, обеспечьте нашем определению монады Reader стандартный интерфейс

ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader g) = Reader $ g . f

-- Штука выше вообще говоря обобщается.
-- Кроме того, что бы поменять локально значение в окружении, можно поменять его тип

local' :: (r -> r') -> Reader r' a -> Reader r a
local' f (Reader g) = Reader $ g . f

-- Монада Writer
-- Я уже показывал простейший пример того, как можно логгировать вычисления, используя, например, пару
-- Кто бы мог подумать, что мы будем работать с очень похожим определением

newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
    fmap f (Writer (x, w)) = Writer (f x, w)

instance (Monoid w) => Applicative (Writer w) where
    pure x = Writer (x, mempty)
    (Writer (f, u)) <*> (Writer (x, v)) = Writer (f x, u `mappend` v)

instance (Monoid w) => Monad (Writer w) where
    return x    = Writer (x, mempty)
    m >>= k     = let   (x, l1) = runWriter m
                        (y, l2) = runWriter $ k x
                    in Writer (y, l1 `mappend` l2)

-- Вам предлагается снова наделить наше определение стандартным интерфейсом

tell :: Monoid w => w -> Writer w ()
tell x = Writer $ ((), x) 

-- ..
-- (v, l) <- listen ..
-- ..

listen :: Monoid w => Writer w a -> Writer w (a, w)
listen (Writer (v, l)) = Writer $ ((v, l), l)  

listens :: Monoid w => (w -> b) -> Writer w a -> Writer w (a, b)
listens f (Writer (v, l)) = Writer $ ((v, f l), l)

censor :: Monoid w => (w -> w) -> Writer w a -> Writer w a
censor f (Writer (v, l)) = Writer $ (v, f l)

-- Монада State. Определение и всякие вспомогательные функции

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f (State sa) = State (\s -> first f $ sa s)

instance Applicative (State s) where
    pure x = State $ \s -> (x, s)
    (State f) <*> (State x) = State $ \s -> let
                                                (fun, s1) = f s
                                                (val, s2) = x s1
                                            in (fun val, s2)

instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State x) >>= k = State $ \s -> let
                                (val, s1) = x s
                                in (runState $ k val) s1

execState :: State s a -> s -> s
execState (State s) seed = snd $ s seed

evalState :: State s a -> s -> a
evalState (State s) seed = fst $ s seed

-- ..
-- s <- get
-- ..
--
get :: State s s
get = State $ \s -> (s, s)

-- ..
-- put 4
-- ..
put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

gets :: (s -> a) -> State s a
gets f = State $ \s -> (f s, s)

-- Решим для начала простую задачку на нее. Дома вам придется сделать аналогичное
-- Напишите функцию, которая считает факториал через монаду State

fac :: Int -> Integer
fac n = fst $ execState (replicateM n facStep) (1, 0)

facStep :: State (Integer, Integer) ()
facStep = do
    (f, n) <- get
    put (f * (n + 1), n + 1)

-- А вот теперь более содержательные примеры. В следующих трех заданиях нужно как-то заиспользовать
-- монаду State
-- Напишите функцию, которая ищет в списке первый элемент, который удовлетворяет предикату
-- Кроме того, предикат выстреливает эффектом.
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p = foldr (\x rec -> p x >>= (\b -> if b then pure $ Just x else rec)) (pure Nothing)

-- Найдите первый повторяющийся элемент в списке.
firstRepeats :: Ord a => [a] -> Maybe a
firstRepeats xs = evalState (findM pred xs) Set.empty
    where
        pred :: Ord a => a -> State (Set.Set a) Bool
        pred x = do
            set <- get
            if (x `Set.member` set) 
            then pure True 
            else do
                put (Set.insert x set)
                pure False


-- Наконец, найдите все повторяющиеся элементы в списке и удалите их
distinct :: Ord a => [a] -> [a]
distinct xs = evalState (filterM pred xs) Set.empty 
    where
        pred :: Ord a => a -> State (Set.Set a) Bool
        pred x = do
            set <- get
            if (x `Set.notMember` set) 
            then do
                put (Set.insert x set)
                pure True
            else 
                pure False
                
-- Немного про IORef. Это такая конструкция, которая позволяет вам заводить мутабельные ссылки
-- в монаде IO. Ха-ха, вот вам и мутабельные данные в функциональном языке!
-- Живут в Data.IORef и предоставляют следующий интерфейс для работы:

-- создание
-- newIORef :: a -> IO (IORef a)

-- чтение
-- readIORef :: IORef a -> IO a

-- запись
-- writeIORef :: IORef a -> a -> IO ()

-- изменение
-- modifyIORef :: IORef a -> (a -> a) -> IO ()

-- Строгая версия modifyIORef
-- modifyIORef’ :: IORef a -> (a -> a) -> IO ()

-- Например:
testIORef :: IO [Integer]
testIORef = do
    x <- newIORef 1
    val1 <- readIORef x
    writeIORef x 41
    val2 <- readIORef x
    modifyIORef x succ
    val3 <- readIORef x
    return [val1,val2,val3]

-- См ../dummy

-- Про случайные числа

-- В хаскелле есть джва способа получить генератор псевдослучайных чисел
-- Первый -- использовать глобальный, который инициализируется системным временем
-- Функция называется getStdGen и живет в System.Random
-- При каждом запуске програмы -- новая уникальная псевдослучайная последовательность

-- Второй - если нужна воспроизводимость. Называется mkStdGen, живет там же, принимает на
-- вход инициализирующее значение

-- Интерфейс примерно такой:
-- randomIO :: IO a
-- random :: RandomGen g => g -> (a, g)
-- randoms :: RandomGen g => g -> [a]

-- Есть функции для получения случайных чисел в диапазоне
-- randomRIO :: (a, a) -> IO a
-- randomR :: RandomGen g => (a, a) -> g -> (a, g)
-- randomRs :: RandomGen g => (a, a) -> g -> [a]

-- Что бы каждый раз получать разные списки с помощью randoms или randomRs
-- возникает необходимость передавать генератор между вычислениями. Напишите функцию,
-- которая прячет это дело в монаде State

{-
randomRState :: (Random a, RandomGen g) => (a, a) -> State g a
randomRState (lo, hi) = do undefined

-- Что бы убедиться, что она работает используйте это:

test :: ([Int],[Int])
test = evalState doWork (mkStdGen 42)

doWork :: State StdGen ([Int], [Int])
doWork = do
    xs <- replicateM 5 $ randomRState (1, 6)
    ys <- replicateM 5 $ randomRState (1, 6)
    return (xs, ys)
-}
-- Если в результате у вас получилось, что функция возвращает разные списки, то все ок


-- Домашнее задание.
-- 0. Используя IORef напишите функцию, которая бы позволяла кодировать императивные циклы а-ля while
-- (1 балл)
while :: IORef a -> (a -> Bool) -> IO () -> IO ()
while ref p action = do 
   v <- readIORef ref
   if p v 
   then do 
        action
        while ref p action
   else return ()

-- Используя ее напишите функцию, которая считает числа Фибоначчи с помощью IORef
-- (1 балл)

fib :: Integer -> IO Integer
fib n = do
    i <- newIORef 0
    f0 <- newIORef 0
    f1 <- newIORef 1
    tmp <- newIORef 0
    while i (< n) ( do
        tmp <- readIORef f0
        fii <- readIORef f1
        writeIORef f0 fii
        modifyIORef' f1 (+ tmp)
        modifyIORef' i (+ 1)
     )
    readIORef f0

-- 1. Смотрите в wc/task.md
