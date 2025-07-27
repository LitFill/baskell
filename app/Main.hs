module Main where

import qualified MyLib (someFunc)

add :: Int -> Int -> Int
add x y = x + y

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    MyLib.someFunc

