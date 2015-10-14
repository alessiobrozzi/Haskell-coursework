-- Informatics 1 - Functional Programming 
-- Tutorial 1
--
-- Due: the tutorial of week 3 (2/3 Oct.)

import Data.Char
import Data.List
import Test.QuickCheck



-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [div x 2 | x <- xs, even x]

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec []			= []
halveEvensRec (x:xs)   | even x		= div x 2 : halveEvensRec xs
	      	       -- | otherwise	= halveEvensRec xs	

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens x = halveEvens x == halveEvensRec x



-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, x >= lo && x <= hi]

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec lo hi []					= []
inRangeRec lo hi (x:xs)	 | x >= lo && x <= hi		= x : inRangeRec lo hi xs
	      	 	 | otherwise  	    		= inRangeRec lo hi xs

-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi x = inRange lo hi x == inRangeRec lo hi x



-- 3. sumPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives xs = length [ x | x <- xs, x > 0 ]

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec []					= 0
countPositivesRec (x:xs)	| x > 0 		= 1 + countPositivesRec xs
		  		| otherwise		= countPositivesRec xs

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives x = countPositives x == countPositivesRec x



-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount x = round ((fromIntegral x) * 0.9)

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher xs =  sum [discount x | x <- xs, sum xs < 22000, x > 0]

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec []							= 0
pennypincherRec (x:xs)	| sum (x:xs) <= 22000 && x > 0			= discount x + pennypincherRec xs
			| otherwise		     	      		= 0

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher x = pennypincher x == pennypincherRec x



-- 5. sumDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits xs = product [digitToInt x | x <- xs, isDigit x]

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec []						= 1
multDigitsRec (x:xs)	| isDigit x				= (digitToInt x) * multDigitsRec xs
	      		| otherwise				= multDigitsRec xs

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits x = multDigits x == multDigitsRec x



-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise xs = head [toUpper x | x <- xs] : tail [toLower x | x <- xs]

-- Helper function
cap :: String -> String
cap xs = [toUpper x | x <- xs, x == head xs]

-- Recursive version
capitaliseRec :: String -> String
capitaliseRec []					= []
capitaliseRec (x:xs) 	| (x:xs) !! 0 == x		= toUpper x : capitaliseRec xs
	      		| otherwise 			= toLower x : capitaliseRec xs
	 

-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise = undefined



-- 7. title

-- List-comprehension version
title :: [String] -> [String]
title xs = head [capitalise x | x <- xs] : tail [capitalise x | x <- xs, length x > 3]




