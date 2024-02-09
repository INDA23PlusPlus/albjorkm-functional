import Data.List
-- Cyber-Clara och anmälningslistorna
-- https://kth.kattis.com/courses/DD2016/plusplus23/assignments/nx4ezb/problems/kth.grupdat.anmalningslistorna

main = interact (solve . words)

joinName (first, last) = first ++ " " ++ last

solve :: [String] -> String
solve [] = ""
solve (count:rest) = solveUsingTwoLists (splitAt (read count) rest)
solveUsingTwoLists (a, b) = solveUsingFullname (map joinName (zip a b))
solveUsingFullname names = (show . length) (nub names)
