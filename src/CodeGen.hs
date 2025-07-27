module CodeGen where

import Language.Haskell.Exts.Simple qualified as H

mkModuleSimple  :: String -> [H.ImportDecl] -> [H.Decl] -> H.Module
mkModuleSimple modName =
    H.Module (Just (H.ModuleHead (H.ModuleName modName) Nothing Nothing)) []

mkTypeSigSimple :: String -> H.Type -> H.Decl
mkTypeSigSimple name = H.TypeSig [H.Ident name]

mkTyFun         :: H.Type -> H.Type -> H.Type
mkTyFun = H.TyFun

mkTyApp         :: H.Type -> H.Type -> H.Type
mkTyApp = H.TyApp

mkTyConSimple   :: String -> H.Type
mkTyConSimple name = H.TyCon (H.UnQual (H.Ident name))

mkNameBind      :: String -> H.Exp -> H.Decl
mkNameBind = H.nameBind . H.name

mkFunSimple     :: String -> [H.Pat] -> H.Rhs -> Maybe H.Binds -> H.Decl
mkFunSimple name pats rhs binds = H.FunBind [H.Match (H.name name) pats rhs binds]

mkVarPat        :: String -> H.Pat
mkVarPat = H.pvar . H.name

mkRhsSimple     :: H.Exp -> H.Rhs
mkRhsSimple = H.UnGuardedRhs

mkOpAppSimple   :: String -> H.Exp -> H.Exp -> H.Exp
mkOpAppSimple op = flip H.infixApp (H.op (H.sym op))

mkVarSimple     :: String -> H.Exp
mkVarSimple = H.var . H.name

-- | main module with @add@ and @main@
-- >>> lines $ H.prettyPrint source
-- [
--     "module Main where",
--     "",
--     "add :: Int -> Int -> Int",
--     "add x y = x + y",
--     "",
--     "main :: IO ()",
--     "main = putStrLn \"Hello, Haskell!\""
-- ]
source          :: H.Module
source = H.Module (Just (H.ModuleHead (H.ModuleName "Main") Nothing Nothing)) [] []
    [ H.TypeSig [H.Ident "add"] (H.TyFun (H.TyCon (H.UnQual (H.Ident "Int"))) (H.TyFun (H.TyCon (H.UnQual (H.Ident "Int"))) (H.TyCon (H.UnQual (H.Ident "Int")))))
    , H.FunBind [H.Match (H.Ident "add") [H.PVar (H.Ident "x"), H.PVar (H.Ident "y")] (H.UnGuardedRhs (H.InfixApp (H.Var (H.UnQual (H.Ident "x"))) (H.QConOp (H.UnQual (H.Symbol "+"))) (H.Var (H.UnQual (H.Ident "y"))))) Nothing]
    , H.TypeSig [H.Ident "main"] (H.TyApp (H.TyCon (H.UnQual (H.Ident "IO"))) (H.TyCon (H.UnQual (H.Ident "()"))))
    , H.FunBind [H.Match (H.Ident "main") [] (H.UnGuardedRhs (H.App (H.Var (H.UnQual (H.Ident "putStrLn"))) (H.Lit (H.String "Hello, Haskell!")))) Nothing]
    ]

intTy           :: H.Type
intTy = mkTyConSimple "Int"

infixr 7 -|>
(-|>) :: H.Type -> H.Type -> H.Type
(-|>) = H.TyFun

-- | simple module example
-- >>> lines $ H.prettyPrint simpleModule
-- [
--     "module MyLib where",
--     "",
--     "mult :: Int -> Int -> Int",
--     "mult a b = a * b",
--     "",
--     "someFunc :: IO ()",
--     "someFunc = putStrLn \"someFunc\""
-- ]
simpleModule :: H.Module
simpleModule = mkModuleSimple "MyLib" []
    [ mkTypeSigSimple "mult" (intTy -|> intTy -|> intTy)
    , mkFunSimple "mult" [mkVarPat "a", mkVarPat "b"] (mkRhsSimple (mkOpAppSimple "*" (mkVarSimple "a") (mkVarSimple "b"))) H.noBinds
    , mkTypeSigSimple "someFunc" (mkTyApp (mkTyConSimple "IO") (mkTyConSimple "()"))
    , mkNameBind "someFunc" (H.appFun (mkVarSimple "putStrLn") [H.strE "someFunc"])
    ]

