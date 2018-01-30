module FromModule (usrFunctions) where

import Data.List
import Language.Haskell.Exts

import DataType
import FuncType


usrFunctions :: String -> Module SrcSpanInfo -> [Func]
usrFunctions path mod = case mod of
                     Module _ (Just moduleHead) _ _ decls -> let (ModuleHead _ (ModuleName _ mdlName) _ _) = moduleHead
                                                                 (funBinds, patBinds') = filterFunPat decls
                                                             in  map (funToFunc path mdlName) funBinds ++ map (patToFunc path mdlName) patBinds'
                     Module _ Nothing           _ _ _    -> error "Non-module Source is Unsupported"
                     _                                   -> error "Unsupported Module Format"

filterFunPat :: [Decl SrcSpanInfo] -> ([Decl SrcSpanInfo], [Decl SrcSpanInfo])
filterFunPat [] = ([], [])
filterFunPat (decl:decls) = case decl of
                               FunBind _ _   -> (decl : funBinds, patBinds')
                               PatBind _ (PVar _ (Ident _ nm))  _ _  ->
                                 if nm == "main"
                                 then binds
                                 else (funBinds, decl : patBinds')
                               _             -> binds
  where binds@(funBinds, patBinds') = filterFunPat decls


funToFunc :: String -> String -> Decl SrcSpanInfo -> Func
funToFunc path mdl funBind = Func nm mdl (sl, el) typ' abTyp path
  where (FunBind info matchs) = funBind
        (SrcSpanInfo {srcInfoSpan = SrcSpan _ sl _ el _}) = info
        nm = funcName (head matchs)
        typ' = funcType path mdl nm
        abTyp = abstSig (snd typ')

funcName :: Match l -> String
funcName match = case match of
                   Match _ (Ident _ nm) _ _ _        -> nm
                   InfixMatch _ _ (Ident _ nm) _ _ _ -> nm


patToFunc :: String -> String -> Decl SrcSpanInfo -> Func
patToFunc path mdl patBind' = Func nm mdl (sl, el) typ' abTyp path
  where (PatBind info pat _ _) = patBind'
        (SrcSpanInfo {srcInfoSpan = SrcSpan _ sl _ el _}) = info
        PVar _ (Ident _ nm) = pat
        typ' = funcType path mdl nm
        abTyp = abstSig (snd typ')
