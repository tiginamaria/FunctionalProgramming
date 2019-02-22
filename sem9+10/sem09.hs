{-# LANGUAGE InstanceSigs, DeriveFoldable, DeriveFunctor #-}
module Sem09 where

import Control.Applicative(ZipList(..), liftA2)
import Data.Monoid

-- traversable
-- 1. traverse . traverse 
-- 2. traverse mappend
--

data Result a = Ok a | Error String deriving (Show, Functor, Foldable)


instance Traversable Result where
   traverse :: (Applicative f) => (a -> f b) -> Result a -> f (Result b)
   traverse f (Ok a) = Ok <$> f a
   traverse _ (Error s) = pure $ Error s

newtype Const x y = Const { getConst :: x } deriving (Eq, Show)

instance Functor (Const c) where
    fmap :: (a -> b) -> Const x a -> Const x b
    fmap _ (Const x) = Const x

instance Foldable (Const c) where
    foldMap :: Monoid m => (a -> m) -> Const c a -> m
    foldMap _ _ = mempty

instance Monoid c => Applicative (Const c) where
    pure :: a -> Const c a
    pure _ = Const mempty
    (<*>) :: Const c (a -> b) -> Const c a -> Const c b
    Const f <*> Const g = Const $ f <> g

instance Traversable (Const c) where
    traverse f (Const x) = pure $ Const x

foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMapDefault f = getConst . traverse (Const . f)

-- see parser.hs


class Functor f => Monoidal f where
    unit :: f ()
    (*&*) :: f a -> f b -> f (a, b)

instance Monoidal [] where
    unit = [()]
    xs *&* ys = [(x, y) | x <- xs, y <- ys]

instance Monoidal ZipList where
    unit = ZipList [()]
    ZipList xs *&* ZipList ys = ZipList $ zip xs ys

unit' :: Applicative f => f ()
unit' = pure ()

pair' :: Applicative f => f a -> f b -> f (a, b)
pair' = liftA2 (,)

pure' :: Monoidal f => a -> f a
pure' x = x <$ unit

ap' :: Monoidal f => f (a -> b) -> f a -> f b
ap' ff fa = (uncurry id) <$> (ff *&* fa)

-- Законы для моноидальных функторов:
-- 1. fst <$> (v *&* unit) = v (Right identity)
-- 2. snd <$> (unit *&* v) = v (Left identity)
-- 3. asl <$> (u *&* (v *&* w)) = (u *&* v) *&* w (Associativity)
-- 4. (f *** g) <$> (u *&* v) = (f <$> u) *&* (g <$> v)
--
-- Где asl, asr и (***) это следующие комбинаторы

asl :: (a, (b, c)) -> ((a, b), c)
asl (x, (y, z)) = ((x, y), z)

asr :: ((a, b), c) -> (a, (b, c))
asr ((x, y), z) = (x, (y, z))

(***) :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
f *** g = \(x, y) -> (f x, g y)


