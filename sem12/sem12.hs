{-# LANGUAGE InstanceSigs
    , FlexibleInstances
    , FlexibleContexts
    , MultiParamTypeClasses 
    , UndecidableInstances #-}

module Sem12 where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Arrow(first, second, (***))
import qualified Data.Set as Set
{-
-- Уважаемые знатоки, верно ли, что эти две программы делают одно и то же?
p1 :: IO ()
p1 =
    let file = "/tmp/file"
    in  do  _ <- writeFile file "abcdef"
            x <- readFile file
            _ <- putStrLn x
            _ <- writeFile file "ghijkl"
            y <- readFile file
            print (x, y)

p2 :: IO ()
p2 =
    let file = "/tmp/file"
        expr = readFile file
    in  do  _ <- writeFile file "abcdef"
            x <- expr
            _ <- putStrLn x
            _ <- writeFile file "ghijkl"
            y <- expr
            print (x, y)

-- Трансформеры монад. На лекции вам показывали различные плюшки для монад
-- как то возможность забесплатно наделить монаду структурой моноида,
-- возможность монадически обрабатывать ошибки  и в конце показали, как комбинировать между собой
-- монадические эффекты на примере трансформера MaybeT, который вы реализовали на лекции

-- Сегодня мы посмотрим на трансформер StateT и, воспользовавшись некоторыми другими монадами,
-- решим примерно те же задачи, что и на прошлой практике, но несколько усложним их постановку

-- Для начала определение MaybeT, стащенное из слайдов

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- Инстансы функтора и аппликативного функтора
-- На лекции вы их не писали, поэтому предлагается для разминки написать их

instance Monad m => Functor (MaybeT m) where
    fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
    fmap = liftM

instance Monad m => Applicative (MaybeT m) where
    pure :: a -> MaybeT m a
    pure = return

    (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
    (<*>) = ap

-- Инстанс монады и MonadTrans

instance MonadTrans MaybeT where
    lift :: Monad m => m a -> MaybeT m a
    lift = MaybeT . liftM Just

instance Monad m => Monad (MaybeT m) where
    return :: a -> MaybeT m a
    return = MaybeT . pure . pure 

    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    (MaybeT m) >>= k = MaybeT $ do
        v <- m
        case v of
            Nothing -> return Nothing
            Just x  -> runMaybeT $ k x

-- Ну а теперь наш трансформер для State
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

-- На прошлом занятии мы писали для монады State инстансы функтора и аппликатива
-- Поэтому у вас не должно вознкнуть особых проблем с тем, что бы написать их для
-- нашего определения StateT

instance Functor f => Functor (StateT s f) where
    fmap :: (a -> b) -> StateT s f a -> StateT s f b
    fmap f (StateT fun) = StateT $ \s -> first f <$> fun s

instance Monad f => Applicative (StateT s f) where
    pure :: a -> StateT s f a
    pure x = StateT $ \s -> pure (x, s)

    (<*>) :: StateT s f (a -> b) -> StateT s f a -> StateT s f b
    StateT fun <*> StateT arg = StateT $ \s -> do
        (f, s') <- fun s
        (x, s'') <- arg s'
        return (f x, s'')

-- Аналогично и для Monad

instance Monad f => Monad (StateT s f) where
    return :: a -> StateT s f a
    return = pure

    (>>=) :: StateT s f a -> (a -> StateT s f b) -> StateT s f b
    (StateT arg) >>= k = StateT $ \s -> do
        (x, s') <- arg s
        (runStateT $ k x) s'
       
instance MonadTrans (StateT s) where
    lift ma = StateT $ \s -> (\x -> (x, s)) <$> ma

-- Тогда определение State s a с предыдущего занятия -- это всего лишь аппликация
-- нашего трансформера к тождественному функтору, что вы и увидели на лекции

type State s = StateT s Identity


-- Вам предлагается наделить наш трансформер стандартным интерфейсом монады State

execStateT' :: Functor f => StateT s f a -> s -> f s
execStateT' (StateT f) s = snd <$> f s

evalStateT' :: Functor f => StateT s f a -> s -> f a
evalStateT' (StateT f) s = fst <$> f s
-}
getT :: Applicative f => StateT s f s
getT = StateT $ \s -> pure (s, s)

putT :: Applicative f => s -> StateT s f ()
putT x = StateT $ \_ -> pure ((), x)
{-
-- Помните задачу с прошлой пары, когда мы убирали все повторяющиеся элементы из списка?
-- Усложним её постановку. Пусть нам не просто нужно почистить список от повторение, но еще
-- и если мы наткнулись на элемент больше какого-либо наперед заданного значения, то наше вычисление
-- завершается неудачей(возвращается Nothing)
-}
distinctF :: Ord a => a -> [a] -> Maybe [a]
distinctF val xs = evalStateT (
    filterM 
        (\x -> do
            if x > val
            then lift $ fail "AAAA"
            else do
                set <- getT
                if (x `Set.notMember` set)
                then do
                    putT (Set.insert x set)
                    pure True
                else do
                    pure False
        ) 
    xs) 
    Set.empty 
-- Домашнее задание
-- 1. Снова про логгирование. На прошлом занятии у нас была монада Writer, на примере которой
-- мы увидели, как можно записывать в лог все, что происходит в наших вычислениях. Рассмотрим следующий
-- тип для логгера

data Logger a = Logger String a deriving (Eq, Show)

-- Реализуйте инстансы всего необходимого для него: функтора, аппликативного функтора и монады

-- (0.33 балла)
instance Functor Logger where
    fmap :: (a -> b) -> Logger a -> Logger b
    fmap f (Logger s a) = Logger s $ f a

-- (0.33 балла)
instance Applicative Logger where
    pure :: a -> Logger a
    pure a = Logger "" a

    (<*>) :: Logger (a -> b) -> Logger a -> Logger b
    (<*>) (Logger s1 fa) (Logger s2 a) = Logger (s1++s2) $ fa a

-- (0.33 балла)
instance Monad Logger where
    return :: a -> Logger a
    return = pure

    (>>=) :: Logger a -> (a -> Logger b) -> Logger b
    (>>=) (Logger s a) f = (Logger s id) <*> (f a)

-- Ну и для добивочки решим следующую задачу. Нам снова дают список и просят вычистить его от повторяющихся
-- элементов, но при этом нужно вести журнал. Если в списке встречается элемент больше заданного, то вычисление
-- завершается с ошибкой и сопровождается соответствующей записью в журнал, при этом запись должна содержать
-- в себе информацию о значении, которое повлекло отмену вычисления. Вдобавок, если элемент удовлетворяет
-- заданному предикату, то об этом так же делается запись в лог.
-- (2 балла)
{-
filterM' :: (Ord a, Monad f, Monad m) => (a -> f (m Bool)) -> [a] -> f (m [a])
filterM' _ [] = pure (pure [])
filterM' p (x : xs) = (\mx -> (++) <$> mx) (((\y -> if y then [x] else []) <*) <* p x) <*> filterM' p xs
 -}
                                         
distinctG :: (Ord a, Show a) => a -> (a -> Bool) -> [a] -> Logger (Maybe [a])
distinctG y p as = evalStateT (fun as) Set.empty where
    fun []     = do { pure $ pure [] }
    fun (x:xs) = do
        set <- getT
        if x > y
            then do 
                StateT $ \s -> Logger ("fail " ++ show x) (Nothing, s)
            else do
            set <- getT
            if (x `Set.notMember` set)
                then do
                putT (Set.insert x set)
                if p x 
                    then (StateT $ \s -> Logger ("ok " ++ show x ++ ", ") (((:) x <$>), s)) <*> fun xs
                    else (StateT $ \s -> Logger ("") (((:) x <$>), s)) <*> fun xs
            else do
                if p x 
                    then (StateT $ \s -> Logger ("ok " ++ show x ++ ", ") ((id), s)) <*> fun xs
                    else (StateT $ \s -> Logger ("") ((id), s)) <*> (fun xs)            
  
-- 2. Сделайте тип данных Result с занятия про траверсы монадой обработки ошибок. Сделайте его представителем классов типов Monad, Alternative, MonadPlus и MonadError String

data Result a = Ok a | Error String deriving (Eq, Show)

instance Functor Result where
    fmap :: (a -> b) -> Result a -> Result b
    fmap _ (Error s) = Error s
    fmap f (Ok x) = Ok $ f x

instance Applicative Result where
    pure :: a -> Result a
    pure = Ok
    (<*>) = ap
    
instance Monad Result where
    return = pure
    (>>=) :: Result a -> (a -> Result b) -> Result b
    (>>=) (Error s) _ = Error s
    (>>=) (Ok x) f = f x
    fail :: String -> Result a
    fail s = Error "Monad.fail error." 

instance Alternative Result where
    empty :: Result a
    empty = fail "Alternative.empty error."
    (<|>) :: Result a -> Result a -> Result a
    (<|>) ok@(Ok x) _ = ok
    (<|>) (Error s) ok@(Ok x) = ok
    (<|>) _ e@(Error s) = e

instance MonadPlus Result where

instance MonadError String Result where
    throwError :: String -> Result a
    throwError = Error
    catchError :: Result a -> (String -> Result a) -> Result a
    catchError (Error s) f = f s
    catchError ok _ = ok
    

-- Для теста используйте например оператор деления
safeDiv :: MonadError String m => Double -> Double -> m Double
x `safeDiv` 0 = throwError "Division by zero"
x `safeDiv` y = pure $ x / y

-- И функцию
example :: Double -> Double -> Result String
example x y = action `catchError` return where
    action = do
        q <- x `safeDiv` y
        guard (q >= 0)
        if (q > 100) then do
            100 <- return q
            undefined
        else do
            pure $ show q 

-- Примеры работы:
-- example 5 2 == Ok "2.5"
-- example 5 0 == Ok "Division by zero."
-- example 5 (-2) == Ok "Alternative.empty error."
-- example 5 0.002 == Ok "Monad.fail error."


-- 3. На основе нашего типа для логгера сконструируйте трансформер LoggerT

newtype LoggerT m a = LoggerT { runLoggerT :: m (Logger a) }

-- Для этого сделайте его монадой для произвольной монады m
-- (0 баллов)

instance Functor m => Functor (LoggerT m) where
    fmap f (LoggerT log) = LoggerT $ ((f <$>) <$>) log

instance Monad m => Applicative (LoggerT m) where
    pure a = LoggerT $ pure $ pure a
    (<*>) (LoggerT flog) (LoggerT log) = LoggerT $ do
        f <- flog 
        x <- log
        return  $ f <*> x

instance Monad m => Monad (LoggerT m) where
    return = pure
    (>>=) (LoggerT log) f = LoggerT $ do
        (Logger s a) <- log 
        (Logger s' b) <- runLoggerT $ f a
        return $ Logger (s ++ s') b
    fail s = LoggerT $ fail s

-- 4. Напишите затем функцию, которая наделяла бы наш трансофрмер стандартным интерфейсом логгера
-- (1 балл)
write2log :: Monad m => String -> LoggerT m () 
write2log = LoggerT . pure . flip Logger () 

-- после чего сделайте обертку над LoggerT Identity
type Logg = LoggerT Identity

-- и напишите запускалку вычислений 
runLogg :: Logg a -> Logger a
runLogg = runIdentity . runLoggerT

-- Примеры:
logTst' :: Logg Integer   
logTst' = do 
    write2log "foo"
    write2log "bar"
    return 42

t1 = (runLogg logTst') == Logger "foobar" 42 -- должно быть верно

-- Еще пример
stLog :: StateT Integer Logg Integer
stLog = do 
    modify (+1)
    a <- get
    lift $ write2log $ show $ a * 10
    put 42
    return $ a * 100

t2 = (runLogg $ runStateT stLog 2) == Logger "30" (300, 42)

-- 5. После чего сделайте его представитем MonadTrans, чтобы в него можно было поднимать вычисления
-- (1 балл)
instance MonadTrans LoggerT where
    lift m = LoggerT $ do
        x <- m
        return $ Logger mempty x

-- пример
logSt :: LoggerT (State Integer) Integer
logSt = do 
  lift $ modify (+1)
  a <- lift get
  write2log $ show $ a * 10
  lift $ put 42
  return $ a * 100

t3 = runState (runLoggerT logSt) 2 == (Logger "30" 300, 42)

-- 6. Ну и напоследок давайте заведем класс типов MonadLogger, который бы выставлял стандратный интерфейс
-- (1 балл)
class Monad m => MonadLogger m where
    writeToLog :: String -> m ()
    logg :: Logger a -> m a

-- Напишите представителей для наших трансформеров LoggerT и StateT

instance Monad m => MonadLogger (LoggerT m) where
    writeToLog = write2log
    logg = LoggerT . return

instance MonadLogger m => MonadLogger (StateT s m) where
    writeToLog = lift . writeToLog
    logg = lift . logg
    
-- пример
logSt' :: LoggerT (State Integer) Integer      
logSt' = do 
    x <- logg $ Logger "BEGIN " 1
    modify (+x)
    a <- get
    writeToLog $ show $ a * 10
    put 42
    writeToLog " END"
    return $ a * 100

t4 = (runState (runLoggerT logSt') 2) == (Logger "BEGIN 30 END" 300,42)

-- что бы он заработал понадобится вот это:
instance MonadState s m => MonadState s (LoggerT m) where
    get   = lift get
    put   = lift . put
    state = lift . state
    
-- 7. Опциональная задача на подумать. 
-- Верно ли, что для произвольных типов состояния s и результата a, тип
-- MaybeT (State s) a это то же самое, что и StateT s Maybe a? 
-- Объясните ваш ответ.
{-
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
|StateT s Maybe a| = |\s -> Maybe (a, s)|
Тогда мощность множества таких отображений |A|^|S| + |S|

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
|MaybeT (State s) a| = |State s (Maybe a)| =  |\s -> (s, Maybe a)| = 
Тогда мощность множества таких отображений (|A| + 1) ^ |S|

-}


