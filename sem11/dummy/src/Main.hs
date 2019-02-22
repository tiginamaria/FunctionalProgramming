module Main where

import Foo
import System.IO

main :: IO ()
main = do
    handle <- openFile "foo.txt" ReadMode
    str <- hGetContents handle
    print str
    hClose handle
