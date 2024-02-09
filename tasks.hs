import qualified Data.Text as T

-- Fibonacci
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Reverse a List
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

-- Average Word Count
avgarr :: [Int] -> Float
avgarr l = fromIntegral (sum l) / fromIntegral (length l)
avgarrwords words = avgarr (map length words) -- use this one if input is array/list
avgwrdlen word_list = avgarrwords (words word_list) -- tokenizes a string for you :)


