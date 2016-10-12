module RPN where

main = do
  putStrLn "Please, introduce a formula"
  line <- getLine
  putStrLn $ inputHandler line
  main

inputHandler :: String -> String
inputHandler = show . solveRPN . words

solveRPN :: (Read n, Fractional n) => [String] -> n
solveRPN = head . foldl evalChar []

evalChar :: (Read n, Fractional n) => [n] -> String -> [n]
evalChar (x:y:xs) "+" = (x + y):xs
evalChar (x:y:xs) "-" = (y - x):xs
evalChar (x:y:xs) "*" = (x * y):xs
evalChar (x:y:xs) "/" = (y / x):xs
evalChar xs numString =
  let num = read numString in num:xs

-- Test with line below as input
-- 10 4 3 + 2 * -
