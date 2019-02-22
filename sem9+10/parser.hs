{-# LANGUAGE InstanceSigs #-}
module Parser where

import Data.Char(isLower, isDigit, digitToInt)
import Control.Applicative

-- Это просто отдельный файл для аппликативного парсера, который вы писали на лекции

data Foo a = Foo a deriving Show
newtype Bar a = Bar {get :: a} deriving Show

newtype Parser tok a = Parser { runParser :: [tok] -> Maybe ([tok], a)}

instance Functor (Parser tok) where
    fmap :: (a -> b) -> Parser tok a -> Parser tok b
    fmap g (Parser p) = Parser f where
        f xs = case p xs of
            Just (cs, c)    -> Just (cs, g c)
            Nothing         -> Nothing

instance Applicative (Parser tok) where
    pure :: a -> Parser tok a
    pure x = Parser $ (\s -> Just (s, x))
    (<*>) :: Parser tok (a -> b) -> Parser tok a -> Parser tok b
    Parser u <*> Parser v = Parser f where
        f xs = case u xs of
            Nothing         -> Nothing
            Just (xs', g)    -> case v xs' of
                Nothing         -> Nothing
                Just (xs'', x)  -> Just (xs'', g x)
    

instance Alternative (Parser tok) where
    empty :: Parser tok a
    empty = Parser $ \_ -> Nothing
    (<|>) :: Parser tok a -> Parser tok a -> Parser tok a
    Parser u <|> Parser v = Parser f where
        f xs = case u xs of
            Nothing -> v xs
            z       -> z


satisfy :: (tok -> Bool) -> Parser tok tok
satisfy pr = Parser f where
    f (t : ts) | pr t   = Just (ts, t)
    f _                 = Nothing

lower :: Parser Char Char
lower = satisfy isLower

char :: Char -> Parser Char Char
char c = satisfy (== c)

digit :: Parser Char Int
digit = digitToInt <$> satisfy isDigit

-- Домашнее задание на аппликативный парсер
-- Везде требуется написать парсеры, которые парсят требуемое
-- (0.5 балла каждое)

-- 0. Научитесь парсить натуральные числа

nat :: Parser Char Int
nat = fmap (foldl (\y x -> x + 10 * y) 0) $ some digit  

-- 1. Научитесь парсить целые числа

int :: Parser Char Int
int = nat <|> (negate) <$ (char '-') <*> nat <|> (id) <$ (char '+') <*> nat 

-- 2. Научитесь парсить что-то между результатом парсера p и парсера q
between :: Parser Char a -> Parser Char b -> Parser Char c -> Parser Char c
between p q r = p *> r <* q

-- 3. Научитесь парсить что-либо внутри круглых скобок
roundBrackets :: Parser Char a -> Parser Char a
roundBrackets = between (char '(') (char ')')

-- 4. Научитесь парсить что-либо внутри фигурных скобок
curlyBrackets:: Parser Char a -> Parser Char a
curlyBrackets = between (char '{') (char '}')

-- 5. Ну вы поняли...
squareBrackets :: Parser Char a -> Parser Char a
squareBrackets = between (char '[') (char ']')

-- 6(вот оно опциональное). Научитесь парсить арифметические выражения(см. тип Expr из шестого домашнего задания)
-- подумайте о скобках
-- (2 балла)

{-
S -> Expr | \epsilon

Expr -> Term ((+ | -) Expr)?

Term -> Factor ((* | /) Term)?

Factor -> | Int | '(' Expr ')'
-}

data Expr = Lit Int
            | BinOp BinOp Expr Expr
            | UnOp UnOp Expr deriving Show

buildBinOp :: Parser Char (Expr -> BinOp -> Expr -> Expr)
buildBinOp = pure (\x y z  -> BinOp y x z) 

buildUnOp :: Parser Char (UnOp -> Expr -> Expr)
buildUnOp = pure (\x y  -> UnOp x y) 

oneOf :: [Char] -> Parser Char Char 
oneOf s = satisfy (flip elem s)

spaces :: Parser Char String
spaces = many $ oneOf [' ', '\t', '\n', '\r', '\f', '\v']

parens :: Parser Char a -> Parser Char a
parens = between (spaces *> char '(') (spaces *> char ')')

data BinOp = Add | Sub | Mul | Div | Mod deriving Show
data UnOp = Minus deriving Show -- унарный минус

addOp :: Parser Char BinOp
addOp =  Sub <$ (spaces *> char '-') <|>  Add <$ (spaces *> char '+')

multOp :: Parser Char BinOp
multOp =  Mul <$ (spaces *> char '*') <|>  Div <$ (spaces *> char '/') <|>  Mod <$ (spaces *> char '%')

unOp :: Parser Char UnOp
unOp = Minus <$ (spaces *> char '-')

expr :: Parser Char Expr
expr = spaces *> buildBinOp <*> term <*> addOp <*> expr <* spaces <|> term <* spaces

term :: Parser Char Expr
term = spaces *> buildBinOp <*> factor <*> multOp <*> term <|> factor <* spaces

factor :: Parser Char Expr
factor = spaces *> buildUnOp <*> unOp <*> (Lit <$> (spaces *> nat)) <|> Lit <$> (spaces *> nat) <|> spaces *> roundBrackets expr

