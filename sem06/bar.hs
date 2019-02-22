module Bar where

-- компилировать:
-- ghc -ddump-deriv -O0 bar.hs
-- выхлоп будет в stdout

infixr 7 :*:

data Cartesian7 a b = a :*: b deriving Show


