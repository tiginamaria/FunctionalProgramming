module Sem12 where

import Control.Monad(ap, replicateM, filterM, liftM)
import qualified Data.Set as Set
import Control.Arrow
import Data.IORef

newtype StateT s m a  =  StateT { runState  :: s -> m (a, s)}

instance  (Functor f) => Functor (StateT s f) where
     fmap f (StateT fun) = StateT $ \s -> first f <$> fun s
     
instance Monad f => Applicative (StateT s f) where
    pure :: a -> StateT s f a
    pure x = StateT $ \s -> pure (x, s)

    (<*>) :: StateT s f (a -> b) -> StateT s f a -> StateT s f b
    StateT fun <*> StateT arg = StateT $ \s -> do
        (f, s') <- fun s
        (x, s'') <- arg s'
        return (f x, s'')
 
instance  (Monad f) => Monad (StateT s f) where
     return = pure
     StateT arg >>= k = StateT $ \s -> do
         (x, s') <- arg s
         (runState $ k x) s'
         
         
execStateT :: (Functor f) => StateT s f a -> s -> f s
execStateT (StateT f) s = snd <$> f s
     
evalStateT :: (Functor f) => StateT s f a -> s -> f a
evalStateT (StateT f) s = fst <$> f s

instance Monad f => Applicative (StateT s f) where
    pure :: a -> StateT s f a
    pure x = StateT $ \s -> pure (x, s)

putT :: (Applicative f) => s -> StateT s f ()
putT s = StateT $ \_ -> pure ((), s)

                
-- Наконец, найдите все повторяющиеся элементы в списке и удалите их
distinctf :: (Ord a) => a -> [a] -> Maybe [a]
distinctf v xs = evalStateT (filterM pred xs) Set.empty 
    where
        pred x = do
            if (x > v)
                then fail "AAA"
            else do
                set <- getT
                if (x `Set.notMember` set) 
                then do
                    putT (Set.insert x set)
                    pure True
                else 
                    pure False
   
   
   
   
                
