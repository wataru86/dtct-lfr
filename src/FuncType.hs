module FuncType (funcType, abstSig, genPropSig) where


import System.IO.Unsafe
import Data.List
import qualified Data.Text as DT
import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Language.Haskell.Interpreter

import ParseMode


funcType :: String -> String -> String -> (String, Type SrcSpanInfo)
funcType path mdl func = case typ' of
                      Right typ -> typ
                      Left  ie  -> 
                        error ("\ninferType ::\n" ++
                          case ie of 
                            UnknownError err  -> err
                            WontCompile  errs -> foldl1 (\x y -> x ++ '\n': y) $
                                                   map errMsg errs
                            NotAllowed   err  -> err
                            GhcException err  -> err
                        )
  where typ' = unsafePerformIO $ runInterpreter $ inferType path mdl func

-- cited |http://d.hatena.ne.jp/keigoi/20100125/1264411453|
inferType :: String -> String -> String -> InterpreterT IO (String, Type SrcSpanInfo)
inferType path mdl func = do
  loadModules $ [path]
  setTopLevelModules [mdl]
  sig <- typeOf func
  let tree = fromParseResult (parseTypeWithMode ParseMode.mode sig)
  return (sig, tree)



nullSSI :: SrcSpanInfo
nullSSI = SrcSpanInfo {
         srcInfoSpan = SrcSpan {
           srcSpanFilename = "" ,
           srcSpanStartLine = 0 , 
           srcSpanStartColumn = 0 ,  
           srcSpanEndLine = 0 ,
           srcSpanEndColumn = 0
         } ,
         srcInfoPoints = []
       }


abstSig :: Type SrcSpanInfo -> (String, String, Int)
abstSig typ' = (genSig abTyp1, genSig abTyp2, snd abTyp1)
  where abTyp1 = abstType False typ'
        abTyp2 = abstType True typ'
        genSig typ = prettyPrint $ fst typ

abstType :: Bool -> Type SrcSpanInfo -> (Type SrcSpanInfo, Int)
abstType frag typ' =
  case typ' of
    TyForall _ _ _ typ -> abstType frag typ
    TyFun _ typ1 typ2  -> let (abTyp1, arg1) = abstType frag typ1
                              (abTyp2, arg2) = abstType frag typ2
                          in ((TyFun nullSSI abTyp1 abTyp2), arg1+arg2) 
    TyTuple _ a typs   -> if frag then varA
                          else (TyTuple nullSSI a (map (fst.(abstType frag)) typs), 1)
    TyList _  typ      -> if frag then varA
                          else (TyList nullSSI (fst (abstType frag typ)), 1)
    TyParArray _ typ   -> if frag then varA
                          else (TyParArray nullSSI (fst (abstType frag typ)), 1)
    TyApp _ a b        -> varA
    TyVar _ _          -> varA 
    TyCon _ qName      -> case qName of
                            UnQual _ (Ident _ "Bool") -> (typ', 1)
                            _                         -> varA
    TyParen _ typ      -> (TyParen nullSSI (fst (abstType frag typ)), 1)
    TyInfix _ _ _ _    -> varA
    TyKind _ typ _     -> abstType frag typ
    TyPromoted _ _     -> varA
    TyEquals _ _ _     -> varA
    TySplice _ _       -> varA
    TyBang _ _ _ typ   -> abstType frag typ
    TyWildCard _ _     -> varA
    TyQuasiQuote _ _ _ -> varA
  where varA = (TyVar nullSSI (Ident nullSSI "a"), 1)


genPropSig :: Type SrcSpanInfo -> String
genPropSig typ' = prettyPrint $ toMonoType $ lastBool typ'


toMonoType :: Type SrcSpanInfo -> Type SrcSpanInfo
toMonoType typ' = rmTopPoly $ exe typ' asstList
  where asstList = classAssts typ'
        exe typ [] = typ 
        exe typ (x:xs) = exe (rename x typ) xs

classAssts :: Type SrcSpanInfo -> [Asst SrcSpanInfo]
classAssts (TyForall _ _ (Just context) typ') =
  case context of
    CxSingle _ asst  -> [asst]
    CxTuple  _ assts -> assts
    CxEmpty  _       -> []
classAssts _ = []


rename :: Asst SrcSpanInfo -> Type SrcSpanInfo -> Type SrcSpanInfo
rename (ClassA _ (UnQual _ (Ident _ nm)) typs) typ' = exe typ'
  where TyVar _ (Ident _ nm'') = head typs
        exe typ' =
          case typ' of
            TyForall _ a b typ    -> TyForall nullSSI a b (exe typ)
            TyFun _ typ1 typ2     -> TyFun nullSSI (exe typ1) (exe typ2) 
            TyTuple _ a typs      -> TyTuple nullSSI a (map exe typs)
            TyList _ typ          -> TyList nullSSI (exe typ)
            TyParArray _ typ      -> TyParArray nullSSI (exe typ) 
            TyApp _ typ1 typ2     -> TyApp nullSSI (exe typ1) (exe typ2) 
            TyVar _ (Ident _ nm') -> 
              if nm' == nm''
              then TyCon nullSSI (UnQual nullSSI (Ident nullSSI (classToType nm)))
              else typ'
            TyParen _ typ         -> TyParen nullSSI (exe typ)
            TyInfix _ typ1 b typ2 -> TyInfix nullSSI (exe typ1) b (exe typ2) 
            TyKind _ typ a        -> TyKind nullSSI (exe typ) a
            TyPromoted _ promoted ->
              case promoted of
                PromotedList _ a typs -> TyPromoted nullSSI (PromotedList nullSSI a (map exe typs))
                PromotedTuple _ typs -> TyPromoted nullSSI (PromotedTuple nullSSI (map exe typs))
            TyEquals _ typ1 typ2  -> TyEquals nullSSI (exe typ1) (exe typ2)  
            TyBang _ a b typ      -> TyBang nullSSI a b (exe typ)
            _                     -> typ'
rename _  _ = error "Unsupported Class Assertion"

classToType :: String -> String
classToType nm = case nm of
                   "Show"       -> "Int"
                   "Read"       -> "Int"
                   "Eq"         -> "Int"
                   "Ord"        -> "Int"
                   "Bounded"    -> "Int"
                   "Enum"       -> "Int"
                   "Num"        -> "Int"
                   "Real"       -> "Int"
                   "Integral"   -> "Int"
                   "Fractional" -> "Float" 
                   "RealFrac"   -> "Float"
                   "Floating"   -> "Float"
                   "RealFloat"  -> "Float"
                   "Monad"      -> "[]"
                   "MonadPlus"  -> "[]"
                   "Functor"    -> "[]"
                   other        -> error $ other ++ " is Unsupported Type Class"

rmTopPoly :: Type SrcSpanInfo -> Type SrcSpanInfo
rmTopPoly typ' =
  case typ' of
            TyForall _ a b typ    -> rmTopPoly typ
            TyFun _ typ1 typ2     -> TyFun nullSSI (rmTopPoly typ1) (rmTopPoly typ2) 
            TyTuple _ a typs      -> TyTuple nullSSI a (map rmTopPoly typs)
            TyList _ typ          -> TyList nullSSI (rmTopPoly typ)
            TyParArray _ typ      -> TyParArray nullSSI (rmTopPoly typ) 
            TyApp _ typ1 typ2     -> TyApp nullSSI (rmTopPoly typ1) (rmTopPoly typ2) 
            TyVar _ _             -> TyCon nullSSI (UnQual nullSSI (Ident nullSSI "Int"))
            TyParen _ typ         -> TyParen nullSSI (rmTopPoly typ)
            TyInfix _ typ1 b typ2 -> TyInfix nullSSI (rmTopPoly typ1) b (rmTopPoly typ2) 
            TyKind _ typ a        -> TyKind nullSSI (rmTopPoly typ) a
            TyPromoted _ promoted ->
              case promoted of
                PromotedList _ a typs -> TyPromoted nullSSI (PromotedList nullSSI a (map rmTopPoly typs))
                PromotedTuple _ typs -> TyPromoted nullSSI (PromotedTuple nullSSI (map rmTopPoly typs))
            TyEquals _ typ1 typ2  -> TyEquals nullSSI (rmTopPoly typ1) (rmTopPoly typ2)  
            TyBang _ a b typ      -> TyBang nullSSI a b (rmTopPoly typ)
            _                     -> typ'


lastBool :: Type SrcSpanInfo -> Type SrcSpanInfo
lastBool (TyForall _ a b typ) = TyForall nullSSI a b (lastBool typ)
lastBool　(TyFun _ typ1 typ2) = TyFun nullSSI typ1 (lastBool typ2)
lastBool _ = TyCon nullSSI (UnQual nullSSI (Ident nullSSI "Bool"))

