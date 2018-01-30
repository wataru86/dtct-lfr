module DLFR (dlfr) where

import System.Timeout
import Control.Monad
import Language.Haskell.Exts

import DataType
import FromModule
import Detect
import ParseMode


maxTime = 30*(10^6)

execute :: Int -> [Func] -> IO ()
execute _    []     = putStr "" 
execute maxN (f:fs) = do
  let msg0 = detect maxN 0 f
      msg1 = detect maxN 1 f
      msg2 = detect maxN 2 f
  t_out <- timeout maxTime msg0
  case t_out of
    Just msg0@(x:_)  -> putStr msg0
    _                -> do
      t_out <- timeout maxTime msg1
      case t_out of
        Just msg1@(x:_)  -> putStr msg1
        _                -> do
          t_out <- timeout maxTime msg2
          case t_out of
            Just msg2  -> putStr msg2
            Nothing    -> putStr ""
  execute maxN fs

dlfr :: Int -> String -> IO ()
dlfr maxN path = do
  psrslt <- parseFileWithMode ParseMode.mode path
  let mdl = fromParseResult psrslt
      usrfuncs = usrFunctions path mdl
  execute maxN usrfuncs