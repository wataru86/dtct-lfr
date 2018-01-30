module Main (main) where 

import System.Environment
import Control.Monad

import DLFR


main = do
 　args <- getArgs
  when (length args /= 2) $
    error "Invalid Command"
  let (s1:s2:_) = args
      maxN = read s1
      srcPath = s2
  dlfr maxN srcPath