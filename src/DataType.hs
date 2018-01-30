module DataType where

import Language.Haskell.Exts


data Func = Func
       { name :: String
       , moduleName :: String
       , location :: (Int, Int)
       , typ :: (String, Type SrcSpanInfo)
       , abstTyp :: (String, String, Int)
       , srcPath :: String 
       }