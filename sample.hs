module Main where

name = "LitFill"

add :: Int -> Int -> Int
add a b = a + b

main :: IO ()
main = do
  putStrLn ("Hello, " ++ name)
  anotherName <- getLine
  putStrLn ("Hello, " ++ anotherName)


