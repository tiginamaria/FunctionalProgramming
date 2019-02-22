module Baz where

foo = seq ((\True x -> "Blah") undefined) 123
bar = seq ((\True -> \x -> "Blah") undefined) 123
