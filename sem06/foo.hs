module Foo where

-- для компиляции:
-- alias ghc-core="ghc -ddump-simpl -dsuppress-idinfo \
-- -dsuppress-coercions -dsuppress-type-applications \
-- -dsuppress-uniques -dsuppress-module-prefixes"
-- ghc-core -O0 foo.hs 
-- выхлоп будет в stdout, осторожно, его может быть много!

foo = \True x -> "Blah"
bar = \True -> \x -> "Blah"
