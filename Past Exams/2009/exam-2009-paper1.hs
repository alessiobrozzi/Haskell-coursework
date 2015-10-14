-- Informatics 1 Functional Programming
-- Final Exam #1 - 7 December 2009
--
-- You do not need to put your name in this file
-- This examination will be marked anonymously


import Data.Char
import Test.QuickCheck


-- Question 1

-- 1a

f :: String -> Bool
f = undefined

-- 1b

g :: String -> Bool
g = undefined

-- 1c

h :: String -> Bool
h = undefined


-- Question 2

-- 2a

p :: [a] -> [a] -> [a]
p xs ys = concat [ [xs !! i] ++ [ys !! i] | i <- [0..(p1 xs ys)] ] ++ drop (length xs) ys ++ drop (length ys) xs
  where
     p1 as bs | length as < length bs = length as - 1
              | otherwise             = length bs - 1

test_p = p "itrev" "nelae" == "interleave" && p "" "justalist" == "justalist" && p [0,0,0,0,0,0] [1,2,3] == [0,1,0,2,0,3,0,0,0]

-- 2b

q :: [a] -> [a] -> [a]
q [] ys                                 = ys
q xs []                                 = xs
q (x:xs) (y:ys)                         = x : y : q xs ys

test_pq :: (Eq a) => [a] -> [a] -> Bool
test_pq xs ys = p xs ys == q xs ys


-- Question 3

type Point = (Int,Int)
data Points = Rectangle Point Point
            | Union Points Points
            | Difference Points Points

-- 3a

inPoints :: Point -> Points -> Bool
inPoints = undefined

-- 3b

showPoints :: Point -> Points -> [String]
showPoints = undefined