{-# LANGUAGE BlockArguments #-}

module AST where
import Text.Printf (printf)

data AST
    = Number Int
    | Str String
    | Ident String
    deriving (Eq, Show)

data Definition
    = Definition
        { name :: String
        , type_ :: Maybe Ty
        , body :: Body
        }
    | OpDef InfixDef
    | TypeDef String String
    | DataDef String [String]
    | NewTypeDef String String
    deriving (Eq, Show)

data Body
    = Func { args :: [String], fnBody :: Expr }
    | Ex Expr
    deriving (Eq, Show)

type Ty = String

data Expr
    = NumLit String
    | Str2 String
    | Ident2 String
    | Appl Expr Expr
    | OpAppl OpAndArgs
    | Lambda [String] Expr
    | Do [Stmt]
    | Is String Expr Expr
    | As Expr String Expr
    | CaseOF Expr [Case]
    deriving (Eq, Show)

type OpAndArgs = (String, Maybe Expr, Maybe Expr)

data Case = Case
    { lhs :: String
    , rhs :: String
    }
    deriving (Eq, Show)

data Stmt
    = Bind String Expr
    | AsBind Expr String
    | DE Expr
    deriving (Eq, Show)

data InfixMode
    = Infix
    | InfixL
    | InfixR
    deriving (Eq, Show)

data InfixDef = InfixDef
    { jenis :: InfixMode
    , precedence :: Int
    , ops :: [String]
    } deriving (Eq, Show)

type ModuleDef = String

type SourceFile = (Maybe ModuleDef, [Definition])

greet :: Expr
greet = OpAppl ("++", Just (Str2 "Hello, "),  Just (Ident2 "name"))

opAppl :: String -> Expr -> Expr -> Expr
opAppl op l r = OpAppl (op, Just l,  Just r)

opApplL :: String -> Expr -> Expr
opApplL op l = OpAppl (op, Just l, Nothing)

opApplR :: String -> Expr -> Expr
opApplR op r = OpAppl (op, Nothing, Just r)

defNonFuncNoType :: String -> Expr -> Definition
defNonFuncNoType name e = Definition name Nothing (Ex e)

defNonFunc :: String -> Ty -> Expr -> Definition
defNonFunc name ty e = Definition name (Just ty) (Ex e)

sample :: SourceFile
sample = (Just "Main",
    [ defNonFuncNoType "name" $ Str2 "LitFill"
    , defNonFunc "luckyNumber" "Int" (NumLit "69")
    , Definition "add" (Just "Int -> Int -> Int") $ Func ["a", "b"] $ opAppl "+" (Ident2 "a") (Ident2 "b")
    , Definition "main" (Just "IO ()") $ Func [] $ Do
        [ DE $ Appl (Ident2 "putStrLn") (opAppl "++" (Str2 "Hello, ") (Ident2 "name"))
        , Bind "anotherName" (Ident2 "getLine")
        , DE $ Appl (Ident2 "putStrLn") (opAppl "++" (Str2 "Hello, ") (Ident2 "anotherName"))
        ]
    ])

printSourceFile :: SourceFile -> String
printSourceFile (mn, defs) = printModuleDef mn ++ "\n" ++ printDefs defs

printDefs :: [Definition] -> String
printDefs = unlines . map printDef

printDef :: Definition -> String
printDef = \case
    Definition n t b -> printNormalDef n t b
    _ -> undefined

printNormalDef :: String -> Maybe Ty -> Body -> String
printNormalDef n mt b =
    printf "%s%s" typedecl bodydecl
  where
    typedecl = maybe "" (printf "%s :: %s\n" n) mt
    bodydecl = case b of
        Func args bd -> fundcl args bd
        Ex e -> normaldecl e
    normaldecl e = printf "%s = %s\n" n (printExpr e)
    fundcl :: [String] -> Expr -> String
    fundcl [] bd' = printf "%s = %s\n" n (printExpr bd')
    fundcl args bd' = printf "%s %s = %s\n" n (unwords args) (printExpr bd')

printExpr :: Expr -> String
printExpr = \case
    NumLit n -> n
    Str2 s -> show s
    Ident2 i -> i
    Appl f a -> printf "(%s %s)" (printExpr f) (printExpr a)
    Do stmts -> unlines $ "do" : map (("  " ++) . printStmt) stmts
    OpAppl oparg -> printOpAndArgs oparg
    _ -> undefined

printOpAndArgs :: OpAndArgs -> String
printOpAndArgs (n, ml, mr) = printf "(%s%s%s)" l n r
  where
    l = maybe "" ((++ " ") . printExpr) ml
    r = maybe "" ((" " ++) . printExpr) mr

printStmt :: Stmt -> String
printStmt = \case
    DE e -> printExpr e
    Bind n e -> printf "%s <- %s" n (printExpr e)
    _ -> undefined

printModuleDef :: Maybe ModuleDef -> String
printModuleDef = maybe "" $ printf "module %s where\n"


