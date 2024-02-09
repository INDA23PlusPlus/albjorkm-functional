-- Problem A - Avstånd till kanten
-- https://kth.kattis.com/courses/DD2016/plusplus23/assignments/nx4ezb/problems/kth.javap.kant

main = interact (unlines . solve . words)

solve [] = []
solve (a:b:rest) = (kanten a b):(solve rest)

dist2char x
    | x <= 9 = show x
    | otherwise = "."

dist r c y x = min (y `min` (r - y + 1)) (x `min` (c - x + 1))

rekt :: Int -> Int -> Int -> Int -> [Char]
rekt r c y x = dist2char (dist r c y x)

sep r y
    | r == y = ""
    | otherwise = "\n"

rad :: Int -> Int -> Int -> [Char]
rad r c y = (concat (map (rekt r c y) [1..c])) ++ sep r y

kanten r c = concat (map (rad r c) [1..r])

