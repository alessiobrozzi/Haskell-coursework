-- Informatics 1 Functional Programming
-- Final Exam #2 - 7 December 2009
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

p :: [a] -> [a]
p = undefined

-- 2b

q :: [a] -> [a]
q = undefined


-- Question 3

type Point = (Int,Int)
data Points = Lines Int Int
            | Columns Int Int
            | Union Points Points
            | Intersection Points Points

-- 3a

inPoints :: Point -> Points -> Bool
inPoints (_,y) (Lines a b)              = y >= a && y <= b
inPoints (x,_) (Columns a b)            = x >= a && x <= b
inPoints (x,y) (Union a b)              = inPoints (x,y) a || inPoints (x,y) b
inPoints (x,y) (Intersection a b)       = inPoints (x,y) a && inPoints (x,y) b

test_ip = inPoints (5,1) (Lines 1 2) == True && inPoints (5,4) (Lines 1 2) == False && inPoints (5,1) (Columns 4 5) == True && inPoints (1,2) (Columns 4 5) == False && inPoints (1,2) (Union (Lines 1 2) (Columns 7 8)) == True && inPoints (5,4) (Union (Lines 1 2) (Columns 7 8)) == False && inPoints (1,2) (Intersection (Lines 2 3) (Columns 0 1)) == True && inPoints (1,1) (Intersection (Lines 2 2) (Columns 2 2 )) ==False 

-- 3b

showPoints :: Point -> Points -> [String]
showPoints (x,y) a = [ nwl i | i <- [0..y] ]
      where
         nwl i = [ nwl2 w | w <- [0..x] ]
             where
                nwl2 w | inPoints (w,i) a = '*'
                       | otherwise        = ' '