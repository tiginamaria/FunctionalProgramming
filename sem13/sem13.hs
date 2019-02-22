{-# LANGUAGE FlexibleContexts #-}

module Sem13 where

import Control.Monad.Except
import Control.Arrow
import Data.Monoid
import Data.Maybe
import Data.List
import Control.Monad.State.Lazy
import Data.Unique
import Data.Char

infixl 2 :@
infixr 3 :->

type Symb = String

-- Терм
data Expr =
    Var Symb
    | Expr :@ Expr
    | Lam Symb Expr
    deriving (Eq,Show,Read)

-- Тип
data Type =
    TVar Symb
    | Type :-> Type
    deriving (Eq,Show)

-- Контекст
newtype Env = Env [(Symb,Type)]
    deriving (Eq,Show)

-- Подстановка
newtype SubsTy = SubsTy [(Symb, Type)]
    deriving (Eq,Show)


-- Вывод типов.
-- Сегодня будет не самая обычная практика по модулю того, что я почти ничего не буду говорить. Нам нужно будет написать алгоритм вывода типов для просто типизированного лямбда-исчисления.
-- Для этого нам понадобятся некоторые определения:
-- Далее, везде, где написан undefined необходимо написать реализацию

-- Всякие вспомогательные штуки
-- Свободные переменные терма(уже писали в большой домашке)
freeVars :: Expr -> [Symb]
freeVars (Var v) = [v]
freeVars (a :@ b) = freeVars a `union` freeVars b
freeVars (Lam v e) = freeVars e \\ [v]

-- Свободные типовые переменные. Есть ли в STT связанные переменные в типах?
freeTVars :: Type -> [Symb]
freeTVars (TVar t) = [t]
freeTVars (a :-> b) = freeTVars a `union` freeTVars b

-- Расширение контекста
extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env e) s t = Env $ e ++ [(s, t)]

-- Свободные типовые переменные в контексте
freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env e) = foldr union [] $ map (freeTVars . snd) e


-- Контекст можно рассматривать еще и как частичную функцию из множества переменных во множество типов
appEnv :: (MonadError String m) => Env -> Symb -> m Type
appEnv (Env e) s 
    | e == []           = throwError $ "There is no variable \"" ++ s ++ "\" in the enviroment."
    | s == fst (head e) = return $ snd $ head e 
    | otherwise         = appEnv (Env $ tail e) s

-- Подстановку можно применять к типу
appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy []) t = t
appSubsTy (SubsTy xs) (TVar s) = if (fst(head xs)) == s
                                     then (snd (head xs))
                                     else appSubsTy (SubsTy(tail xs)) (TVar s)
appSubsTy subs (a:-> b) = ((appSubsTy subs a) :-> (appSubsTy subs b))

-- И к контексту
appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv subs (Env e) = Env $ foldr (\(s, t) xs -> [(s, appSubsTy subs t)] `union` xs) [] e

-- Их можно комбинировать
composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy s1@(SubsTy xs1) s2@(SubsTy xs2) = 
    SubsTy $ map (doSubs s1 . doSubs s2) uniVar where
    doSubs subs = \(s, t) -> (s, appSubsTy subs t)
    uniVar = map (\x -> (x, TVar x)) $ fst (unzip xs1) `union` fst (unzip xs2)

    
-- И подстановки образуют моноид

instance Semigroup SubsTy where
    (<>) = composeSubsTy

instance Monoid SubsTy where
    mempty = SubsTy []
    mappend = composeSubsTy
    

-- Наконец, реализуйте алгоритм унификации для двух типов
unify :: (MonadError String m) => Type -> Type -> m SubsTy
unify (TVar s1) (TVar s2)
    | s1 == s2   = return $ SubsTy []
    | otherwise  = return $ SubsTy [(s1 , TVar s2)]
    
unify (TVar s) t
    | s `elem` freeTVars t  = throwError $ "Can't unify  ("++(show s)++") with ("++(show t)++")!"
    | otherwise             = return $ SubsTy [(s, t)]
    
unify (a :-> b) (TVar s)  = unify (TVar s) (a :-> b)

unify (a :-> b) (a' :-> b') = do
    ub <- unify b b'
    uba <- unify (appSubsTy ub a) (appSubsTy ub a')
    return $ uba `mappend` ub
    
    
-- Реализуйте алгоритм составления системы ограничений для терма в заданном контексте и начальном типе. Обратите особое внимание на случаи аппликации и абстракции

getFreshTVar :: MonadState Integer m => m Type
getFreshTVar = do
  n <- get
  modify (+1)
  return $ TVar $ show n

equations :: (MonadError String m) => Env -> Expr -> Type -> m [(Type,Type)]
equations e expr t = evalStateT (equations' e expr t) 1

equations' :: (MonadError String m) => Env -> Expr -> Type -> StateT Integer m [(Type,Type)]
equations' e v@(Var x) t = do
    t' <- appEnv e x
    return [(t, t')]
    
equations' e (a :@ b) t = do
    ft <- getFreshTVar 
    eq1 <- equations' e a (ft :-> t)
    eq2 <- equations' e b ft 
    return $ eq1 `union` eq2
    
equations' e (Lam s expr) t = do
    ft1 <- getFreshTVar
    ft2 <- getFreshTVar
    eq <- equations' (extendEnv e s ft1) expr ft2
    return $ eq `union` [(ft1 :-> ft2, t)]

-- Воспользовавшись им, напишите алгоритм поиска главной пары -- главного типа и контекста, в котором верно утверждение о типизации данного терма

principlePair :: (MonadError String m) =>  Expr -> m (Env,Type)
principlePair expr = do
    let e = Env $ map (\x -> (x,TVar $ x ++ "'")) (freeVars expr)
    let t = TVar "var0"
    eq <- equations e expr t
    let (tl, tr) = foldr (\x y -> ((fst x :-> fst y),(snd x :-> snd y))) (head eq) (tail eq)
    subs <- unify tl tr
    return $ (appSubsEnv subs e, appSubsTy subs t)
    
