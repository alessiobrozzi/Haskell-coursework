-- Informatics 1 Functional Programming
-- Resit Exam - 20 August 2010
--
-- You do not need to put your name in this file
-- This examination will be marked anonymously


import Data.Char
import Test.QuickCheck

-- Question 1

-- 1a

f :: String -> Bool
f =  undefined

-- 1b

g :: String -> Bool
g =  undefined

-- 1c

h :: String -> Bool
h =  undefined


-- Question 2

-- 2a

p :: a -> [a] -> [a]
p x ys = concat [ [x] ++ [ ys !! i ] | i <- [0..(length ys - 1)] ] ++ [x]

test_p = p 'x' "ABCD" == "xAxBxCxDx" && p 'a' "XY" == "aXaYa" && p '-' "Hello" == "-H-e-l-l-o-" && p '-' "" == "-" && p 0 [1,2,3,4,5] == [0,1,0,2,0,3,0,4,0,5,0]

-- 2b

q :: a -> [a] -> [a]
q x []           = [x]
q x (y:ys)       = [x] ++ [y] ++ q x ys

test_pq :: (Eq a) => a -> [a] -> Bool
test_pq x ys = p x ys == q x ys


-- Question 3

type Point  = (Int,Int)
data Points = X
            | Y
            | DX Int Points
            | DY Int Points
            | U Points Points

-- 3a

inPoints :: Point -> Points -> Bool
inPoints (_,b) X               = b == 0
inPoints (a,_) Y               = a == 0
inPoints (a,b) (DX dx p)       = inPoints (a - dx,b) p
inPoints (a,b) (DY dy p)       = inPoints (a,b - dy) p
inPoints x (U p q)             = inPoints x p || inPoints x q

test_ip = inPoints (3,0) X == True && inPoints (0,1) Y == True && inPoints (3,3) (DY 3 X) == True && inPoints (2,1) (DX 2 Y) == True && inPoints (3,0) (U X Y) == True && inPoints (0,1) (U X Y) == True && inPoints (3,3) (U (DY 3 X) (DX 2 Y)) == True && inPoints (2,1) (U (DY 3 X) (DX 2 Y)) == True && inPoints (3,0) (U (U X Y) (U (DX 2 Y) (DY 3 X))) == True && inPoints (0,1) (U (U X Y) (U (DX 2 Y) (DY 3 X))) == True && inPoints (3,3) (U (U X Y) (U (DX 2 Y) (DY 3 X))) == True && inPoints (2,1) (U (U X Y) (U (DX 2 Y) (DY 3 X))) == True && inPoints (1,1) X == False && inPoints (1,1) Y == False && inPoints (1,1) (DY 3 X) == False && inPoints (1,1) (DX 2 Y) == False && inPoints (1,1) (U X Y) == False && inPoints (1,1) (U X Y) == False && inPoints (1,1) (U (DY 3 X) (DX 2 Y)) == False && inPoints (1,1) (U (DY 3 X) (DX 2 Y)) == False && inPoints (1,1) (U (U X Y) (U (DX 2 Y) (DY 3 X))) == False

-- 3b

countAxes :: Points -> Int
countAxes X             = 1
countAxes Y             = 1
countAxes (DX _ p)      = countAxes p
countAxes (DY _ p)      = countAxes p
countAxes (U p q)       = countAxes p + countAxes q

test_ca = countAxes X == 1 && countAxes Y == 1 && countAxes (U X Y) == 2 && countAxes (U (DY 3 X) (DX 2 Y)) == 2 && countAxes (U (U X Y) (U (DX 2 Y) (DY 3 X))) == 4 && countAxes (U (U X Y) X) == 3


