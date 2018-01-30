{-# LANGUAGE StandaloneDeriving , DeriveDataTypeable #-}

module EqTest (isEqual) where


import System.IO.Unsafe
import Language.Haskell.Interpreter
import Test.QuickCheck.Test

import DataType
import FuncType
import ParseMode

import Data.Typeable
deriving instance Typeable Result


isEqual :: Func -> Func -> IO Bool
isEqual f1 f2 = do
  res <- runInterpreter $ equivalent f1 f2
  case res of
    Left  _      -> return False
    Right result ->
      case result of
        Success {} -> return True
        _          -> return False
  
equivalent :: Func -> Func -> Interpreter Result
equivalent f1 f2  = do
    loadModules [path]
    setTopLevelModules [mdl]
    setImports [mdl', "Test.QuickCheck.Test", "Text.Show.Functions"]
    action <- interpret qcExpr (as :: IO Result)
    lift action
  where qcExpr = "quickCheckWithResult " ++ str ++ " ((\\" ++ argNames
                   ++ " -> " ++ nm ++ " " ++ argNames
                   ++ " == " ++ mdl' ++ '.' : nm' ++ " " ++ argNames
                   ++ ") :: " ++ typeNote ++ ")"
        str = "Args {replay = Nothing, maxSuccess = 100, maxDiscardRatio = 10, maxSize = 100, chatty = False, maxShrinks = maxBound}"
        (Func nm  mdl  _ (_, ast) (_, _, termN) path) = f1
        (Func nm' mdl' _ _        _          _   ) = f2
        typeNote = genPropSig ast
        argNames = genArgNames (termN - 1)

genArgNames :: Int -> String
genArgNames 0 = ""
genArgNames n = "x" ++ show n ++ " " ++ genArgNames (n-1)

