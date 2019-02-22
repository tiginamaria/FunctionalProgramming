
{-# LANGUAGE FlexibleContexts #-}
module Sem10 where

import Control.Applicative(liftA2)
import Control.Monad hiding (replicateM, foldM, mapM, filterM)

-- Монады.
-- Можно сколько угодно рассуждать о том, что же такое монада. Есть теоретико категориальное определение,
-- есть более программистское, которое вы видели на лекции, есть издевательское("монада -- это всего лишь моноид
-- в категории эндофункторов"), но мы не будем этого делать.
-- Стремный пример для затравки. Чему равно значение выражения(да, я решил их вернуть)?
-- [1,2] >>= (\n -> ['a', 'b'] >>= \c -> return (n, c))
-- Как записать его в do-нотации?


-- iffy (Just True) (Just 1) Nothing == Just 1
iffy :: Applicative f => f Bool -> f a -> f a -> f a
iffy fb ft fe = pure (\b x y -> if b then x else y) <*> fb <*> ft <*> fe

miffy :: Monad f => f Bool -> f a -> f a -> f a
miffy fb ft fe = fb >>= (\b -> if b then ft else fe)
 
-- Напишите реализации функций filter и replicate через do-нотацию и монаду списка

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = do 
    x <- xs
    if p x then pure x else fail "AAA"


filter'' p xs = concatMap (\x -> if p x then [x] else []) xs


replicate' :: Int -> a -> [a]
replicate' n x = [1..n] >> pure x

-- Что вернет вызов и почему?
-- GHCi> do {b <- [True, False, True]; when b []; return 42})

-- Лирическое отступление. В полном определении класса типов Traversable есть
-- монадические аналоги traverse и sequenceA. Первый зовется mapM, а второй -- sequence
-- Их определения до ужаса просты, они просто вызывают соответствующих аппликативных родителей

-- Кроме того, в Control.Monad живут монадические обобщения некоторых функций над списками
-- Например zipWithM, replicateM, filterM и foldM

zipWithM :: (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequenceA (zipWith f xs ys)

replicateM :: (Applicative m) => Int -> m a -> m [a]
replicateM cnt0 f =
    loop cnt0
        where
            loop cnt
                | cnt <= 0  = pure []
                | otherwise = liftA2 (:) f (loop (cnt - 1))

filterM :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterM p = foldr (\ x -> liftA2 (\ flg -> if flg then (x:) else id) (p x)) (pure [])

foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM f z0 xs = foldr f' return xs z0
  where f' x k z = f z x >>= k

-- Выше приведены их определения, стащенные из хугла. В новой версии компилятора у первых трех
-- функций контекст Monad был обобщен до Applicative, а у foldM список обобщился до произвольного
-- Foldable. Давайте посмотрим на примеры использования этих чуваков:

-- Повызывайте в GHCi:
-- zipWithM (\x y -> Just (x + y)) [1,2,3] [4, 5]
-- zipWithM (\x y -> (show x ++ "+" ++ show y ++ ";", x + y)) [1,2,3] [4,5,6]
-- replicateM 2 (ZipList [1,2,3])
-- filterM (Just . odd) [1,2,3]
-- traverse (Just . odd) [1,2,3]
-- filterM (\_ -> [True, False]) [1,2,3]

-- Имея foldM можно, например суммировать числа, контролируя выход за диапазон

isSmall :: (Ord a, Num a) => a -> Bool
isSmall x = (x < 255) && (x >= -256)

safePlus :: (Num a, Ord a) => a -> a -> Maybe a
safePlus x y =  let 
                    res = x + y 
                in if isSmall res then Just res else Nothing




isTiny :: (Ord a, Num a) => a -> Bool
isTiny x = x > (-128) && x < 127

(?+?) :: (Ord a, Num a) => a -> a -> Maybe a
x ?+? y = let res = x + y in if isTiny res then Just res else Nothing

infixl 6 ?+?

-- Повызывайте в GHCi:
-- 100 ?+? 1
-- 100 ?+? 50
-- foldM (?+?) 0 [1..15]
-- foldM (?+?) 0 [1..16]

-- Как связаны монады, функторы и аппликативные функторы.
-- Давайте покажем, что имея на руках монаду, можно без особого труда выразить
-- методы Functor и Applicative

-- Выразите fmap через (>>=) и return
myfmap :: Monad m => (a -> b) -> m a -> m b
myfmap f ma = ma >>= (pure . f)

-- И через do-нотацию
myfmap' :: Monad m => (a -> b) -> m a -> m b
myfmap' f ma = do
    a <- ma
    pure $ f a

-- То же самое с Applicative, напишите (<*>) используя return и (>>=)

myapp :: Monad m => m (a -> b) -> m a -> m b
myapp mf ma = mf >>= \f -> ma >>= \a -> pure $ f a

-- И do-нотацию
myapp' :: Monad m => m (a -> b) -> m a -> m b
myapp' mf ma = do
    f <- mf
    a <- ma
    pure $ f a

-- Заметим, что выше, вообще говоря, надо бы еще показать, что законы функторов следуют из
-- законов монад, равно как и законы аппликативных функторов. Вместо этого давайте рассмотрим вот такие
-- вот монадические комбинаторы, определенные в Control.Monad

-- Этот зовется Kleisli composition, но я буду звать его рыбкой
-- Он позволяет задать композицию стрелок Клейсли
-- >=> :: Monad m -> (a -> m b) -> (b -> m c) -> (a -> m c)

-- У него есть повернутый брат-близнец
-- <=< :: Monad m -> (b -> m c) -> (a -> m b) -> (a -> m c)
-- <=< = flip (>=>)

-- И, наконец:
-- join :: Monad m -> m (m a) -> m a

-- Выразите (>=>) через >>=
fish :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
fish ka kb a = ka a >>= kb

-- Выразите join через (>>=)
myjoin :: Monad m => m (m a) -> m a
myjoin mma = mma >>= id

-- Выразите (>=>) через join и fmap
fish' :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
fish' ka kb a = join (kb <$> ka a)

-- Домашнее задание. Траверсы
--
-- 0. Я давно должен рассказать вам про композицию типов. Рассмотрим следующий трехпараметрический оператор над типами

newtype Cmps f g x = Cmps { getCmps :: f (g x) } deriving Show


-- Какой у него кайнд?
--Cmps :: (* -> *) -> (* -> *) -> * -> *

-- Приведите примеры _типов_, сконструированных с помощью Cmps, и _термов_, которые имеют типы, сконструированные вами

mytype :: Num x => Cmps Maybe [] x  
mytype = Cmps $ Just $ [44]

-- Рассмотрим следующий вызов

ffmap h =  getCmps . fmap h . Cmps  

-- Что не нравится компилятору, когда он пытется проверить тип ffmap?
-- Сделайте так, что бы код выше можно было _вызывать_
-- Для этого нужно _внимательно_ прочитать сообщение об ошибке
-- (1 балл)

instance (Functor f, Functor g) => Functor (Cmps f g) where
    fmap f (Cmps c) = Cmps $ fmap (fmap f) c 
    
-- После этого, сделайте этот тип представителем Foldable
-- (1 балл)

instance (Foldable f, Foldable g) => Foldable (Cmps f g) where
   foldMap f (Cmps c) = foldMap (foldMap f) c

-- И Traversable
-- (1 балл)

instance (Traversable f, Traversable g) => Traversable (Cmps f g) where
    traverse f (Cmps c) = Cmps <$> traverse (traverse f) c

-- 1. Сделайте двоичное дерево представителем класса Traversable. В каком порядке надо его обходить,
-- что бы выполнялся traverse Identity = Identity?
-- Пример работы:
-- traverse (\x -> if odd x then Right x else Left x) (Branch (Branch Leaf 1 Leaf) 3 Leaf) == Right (Branch (Branch Leaf 1 Leaf) 3 Leaf)
-- traverse (\x -> if odd x then Right x else Left x) (Branch (Branch Leaf 1 Leaf) 2 Leaf) == Left 2

data Tree a = Leaf | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
    fmap f Leaf = Leaf 
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)
    
instance Foldable Tree where
    foldMap f Leaf = mempty
    foldMap f (Branch l a r) = (foldMap f l) <> f a <> (foldMap f r)
    
instance Traversable Tree where
    traverse f (Leaf) = pure Leaf
    traverse f (Branch l a r) = Branch <$> (traverse f l) <*> (f a) <*> (traverse f r)
    
-- 2. Смотри parser.hs
--
-- 3. Моноидальные функторы
--
-- Напишите представителя Monoidal для Maybe. Докажите, что ваша реализация удовлетворяет законам(см. sem09.hs)
-- (1 балл за рабочий инстанс + 1 балл за доказательство каждого закона)
--

class Functor f => Monoidal f where
    unit :: f ()
    (*&*) :: f a -> f b -> f (a, b)

instance Monoidal Maybe where
    unit = Just ()
    (*&*) (Just a) (Just b ) = Just (a, b)
    (*&*) _ _ = Nothing
     
{- 

1. fst <$> (v *&* unit) = v                                     (Right identity)
   
   (v *&* unit) = 
   case v of
       Nothing -> Nothing
       Just u  -> Just (u, ())
       
   fst <$> Nothing = Nothing
   fst <$> Just (u, ()) = Just fst (u, ()) = Just u = v
    

2. snd <$> (unit *&* v) = v                                  (Left identity)
  
   (unit *&* v) = 
   case v of
       Nothing -> Nothing
       Just u  -> Just ((), u)
       
   snd <$> Nothing = Nothing
   snd <$> Just ((), u) = Just snd ((), u) = Just u = v
    

3. asl <$> (u *&* (v *&* w)) = (u *&* v) *&* w                (Associativity)

   Если u или v или w = Nothing, то asl (u *&* (v *&* w)) = asl Nothing = Nothing |
                                 и  (u *&* v) *&* w = Nothing                     | => asl (u *&* (v *&* w)) = Nothing = (u *&* v) *&* w
   Иначе:
   (Just u' *&* (Just v' *&* Just w')) = (Just u' *&* Just (v', w')) = Just (u', (v', w'))
   ((Just u' *&* Just v') *&* Just w')) = (Just (u', v') *&* Just w') = Just ((u', v'), w')
   asl <$> Just (u', (v', w')) = Just asl (u', (v', w')) = Just ((u', v'), w')) = (u *&* v) *&* w

4. (f *** g) <$> (u *&* v) = (f <$> u) *&* (g <$> v)
   Если u или v = Nothing(пусть u), то (f *** g) <$> (u *&* v) = (f *** g) <$> Nothing = Nothing |
                                    и  (f <$> u) *&* (g <$> v) = Nothing *&* (g <$> v) = Nothing | => (f *** g) <$> (u *&* v) = Nothing = (f <$> u) *&* (g <$> v)
   
   (f *** g) <$> (Just u' *&* Just v') = (f *** g) <$> Just (u', v') = Just (f *** g) (u', v') = Just (f u', g v')
   (f <$> Just u') *&* (g <$> Just v') = Just f u' *&* Just g v' = Just (f u', g v') =  (f *** g) <$> (Just u' *&* Just v')
--
-}

-- 4. Связь между моноидальными и аппликативными функторами
-- Докажите, что законы класса типов Applicative под названием Homomorphism и Interchange следуют из моноидальных
-- (1 балл за доказательство каждого закона)
 
{-  

u <*> pure x = pure ($ x) <*> u                                    (Interchange)

u <*> pure x = 
(uncurry id) <$> (u *&* (const x <$> unit)) = 
(uncurry id) <$> ((fst <$> (u *&* unit)) *&* (const x <$> unit)) =
(uncurry id) <$> (fst *** const x) <$> ((u *&* unit) *&* unit) = 
(uncurry id) <$> (\(a, b) -> (fst a, const x b)) <$> ((u *&* unit) *&* unit)) = 
(\(a, b) -> (uncurry id) (fst a, const x b)) <$> ((u *&* unit) *&* unit)) = 
($ x) .fst .fst <$> ((u *&* unit) *&* unit) = 
($ x) .fst <$> (u *&* unit)
($ x) <$> u


pure ($ x) <*> u = 
(uncurry id) <$> ((const ($ x) <$> unit) *&* u) = 
(uncurry id) <$> ((const ($ x) <$> unit) *&* (snd <$> (unit *&* u))) =
(uncurry id) <$> (const ($ x) *** snd) <$> (unit *&* (unit *&* u))) = 
(uncurry id) <$> (\(a, b) -> (const ($ x) a, snd b)) <$> (unit *&* (unit *&* u))) = 
(\(a, b) -> (uncurry id)(const ($ x) a, snd b)) <$> (unit *&* (unit *&* u))) = 
($ x) .snd .snd <$> (unit *&* (unit *&* u)) = 
($ x) <$> u



pure f <*> pure x =                                              (Homomorphism)
(uncurry id) <$> ((const f <$> unit) *&* (const x <$> unit)) =
(uncurry id) <$> (const f *** const x) <$> (unit *&* unit) = 
(uncurry id) <$> (\(a, b) -> (const f a, const x b)) <$> (unit *&* unit) = 
(\(a, b) -> (uncurry id)(f, x)) <$> (unit *&* unit) = 
(const f x) <$> (unit *&* unit) <=> 
(const f x) <$> unit = 
pure (f x) 
-}

-- 5. Монады. Список как монада
-- Напишите функцию, которая ищет список всех значений с заданным ключом в заданном ассоциативном списке(списке пар "ключ-значение")
-- (1 балл)

lookups :: (Eq k) => k -> [(k, v)] -> [v]
lookups x kvs = concatMap (\(k,v) -> if k == x then [v] else []) kvs

lookups' :: (Eq k) => k -> [(k, v)] -> [v]
lookups' x kvs = do 
                    (k,v) <- kvs
                    if k == x 
                    then pure v 
                    else fail ""

-- Напишите функцию, которая раскладывает положительное целое число на три сомножителя всевозможными способами. 
-- Тройки должны быть уникальны и отсортированы лексикографически в порядке неубывания
-- (1 балл)

factor3 :: Integer -> [(Integer, Integer, Integer)]
factor3 x = do
                x1 <- [1..x]
                x2 <- [x1..x]
                x3 <- [x2..x]
                if (x1 * x2 * x3 == x) 
                then pure (x1, x2, x3) 
                else fail ""  
                
-- 6. Монадные законы. На занятии мы выразили операторы композиции Клейсли через >>= и join через >>=
-- Переведите законы монад из формулировок через (>>=) и return, в формулировку через (>=>) и return
-- Сделайте то же самое, но теперь уже в формулировку через join (и fmap) и return. Для второго полезно
-- помнить закон функтора и "свободные теоремы" для типов return и join

				{-
				-- Этот зовется Kleisli composition, но я буду звать его рыбкой
				-- Он позволяет задать композицию стрелок Клейсли
				-- >=> :: Monad m -> (a -> m b) -> (b -> m c) -> (a -> m c)

				-- У него есть повернутый брат-близнец
				-- <=< :: Monad m -> (b -> m c) -> (a -> m b) -> (a -> m c)
				-- <=< = flip (>=>)

				-- И, наконец:
				-- join :: Monad m -> m (m a) -> m a

				-- Выразите (>=>) через >>=
				fish :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
				fish ka kb a = ka a >>= kb

				-- Выразите join через (>>=)
				myjoin :: Monad m => m (m a) -> m a
				myjoin mma = mma >>= id

				-- Выразите (>=>) через join и fmap
				fish' :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
				fish' ka kb a = join (kb <$> ka a)
				-}

-- Формулировки монадных законов:

-- return x >>= k   = k x                         | return >=> k      = k                  | join . return = id
-- m >>= return     = m                           | m >=> return      = m                  | join . fmap return = id
-- m >>= u >>= v    = m >>= (\x -> u x >>= v)     | (u >=> v) >=> w = u >=> (v >=> w)      | join . join = join . fmap join

{-
(f >=> g x = f x >>= g)

return x >>= k = k x
return >=> k x = k x
return >=> k = k

m >>= return = m
(m >=> return) x = m x
m x >>= return = m x

((u >=> v) >=> w) x = u >=> (v >=> w) x
((u >>= v) x) >=> w) = u x >>= (v >=> w)
((u x >>= v) >=> w) = u x >>= (v >=> w)
((u x >>= v) >=> w) = u x >>= (\y -> v y >=> w)

-}
--_________________________________________________________________

-- Законы функторов:
-- fmap id      = id
-- fmap (f . g) = fmap f . fmap g

-- Свободные теоремы для типов return и join:
-- return . f           = fmap f . return
-- join . fmap (fmap f) = fmap f . join
-- (1.5 балла за каждую формулировку законов)
{-

Перевыразим join и fmap через >>=
fmap k m = m >>= return . k
join m   = m >>= id
m >>= k  = join (fmap m x)

join . return x
= return x >>= id
= id x
= x

join . fmap return m
= (m >>= return . return) >>= id
= m >>= \ x -> return (return x) >>= id
= m >>= \ x -> return x
= m >>= return
= m

join (join m)
= (m >>= id) >>= id
= m >>= \ x -> x >>= id
= m >>= \ x -> join x
= m >>= \ x -> return (join x) >>= id
= (m >>= return . join) >>= id
= join (fmap join m)

-}
