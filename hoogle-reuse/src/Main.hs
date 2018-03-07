
module Main(main) where

import System.Environment
import System.IO
import HoogleReuse


main :: IO ()
main = do
    hSetEncoding stdout utf8
    hoogle =<< getArgs
