module Detect (detect) where

import System.IO.Unsafe
import System.Timeout
import Control.Monad
import Language.Haskell.Exts

import HoogleReuse
import DataType
import FuncType
import EqTest
import ParseMode


detect :: Int -> Int -> Func -> IO String
detect maxN dMode f@(Func nm mdl loc (sig, ast) (sig', sig'', termN) _)
 | termN >= 2  = do
    let querys =
         case dMode of 
               0 -> [sig]
               1 -> [sig']
               2 -> [sig'']
    cmdLine <- getCmdLine querys
    targets <- actionSearch' cmdLine
    let tarFuncs = take maxN $ map (toFunc.(targetResultDisplay False)) targets
    exeTest f tarFuncs
 | otherwise   = return ""
      
toFunc :: String -> Func
toFunc str = Func nm mdl loc (sig, ast) abTyp path
  where (mdl, (_:rem)) = break (== ' ') str
        (nm, rem') = break (== ' ') rem
        loc = (0, 0)
        sig = drop 4 rem'
        ast = fromParseResult $ parseTypeWithMode ParseMode.mode sig
        abTyp = abstSig ast
        path = ""

exeTest :: Func -> [Func] -> IO String
exeTest _ [] = return ""
exeTest f (g:gs) = do
    equal <- isEqual f g
    if equal then return msg
    else exeTest f gs
  where (Func nm  mdl  (st, en) (sig　, _) _ _) = f
        (Func nm' mdl' _        (sig', _) _ _) = g
        msg = mdl ++ ".hs: " 
                ++ "line " ++ lineN ++ ": \n"
                ++ nm ++ " :: " ++ sig ++ "\n  <=> "
                ++ mdl' ++ "." ++ nm' ++ " :: " ++ sig'
                ++ "\n\n"
        lineN 
         | st == en   = show st
         | otherwise  = show st ++ "-" ++ show en
  