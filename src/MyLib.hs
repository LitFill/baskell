module MyLib (someFunc) where

import Language.Haskell.Exts.Simple qualified as H
import Language.Haskell.Exts.Simple.Syntax qualified as H

ident = H.simpleFun (H.name "identity") x (H.var x)
  where x = H.name "x"

someFunc :: IO ()
someFunc = putStrLn "someFunc"
