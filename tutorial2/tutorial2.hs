-- Informatics 1 - Functional Programming 
-- Tutorial 2
--
-- Week 4 - due: 9/10 Oct.

import Data.Char
import Data.List
import Test.QuickCheck


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate n []					= []
rotate n (x:xs)	| n < 0 || n > 26		= "n not appliable"
       	 	| ord x >= 65 && ord x <= 90	= chr (ord x + n) : rotate n xs
		| ord x >= 97 && ord x <= 122	= chr (ord x + n) : rotate n xs

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3. 


makeKey :: Int -> [Char] -> [(Char, Char)]
makeKey n []   	  	    = []
makeKey n (c:cs)	    = (c, (chr ((ord c) + n))) : makeKey n cs

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp c xs = head ([ y | (x,y) <- xs, c == x] ++ [c])

lookUp2 c xs = sel c [b | (a,b) <- xs, a == c]
       	       where sel c [x] = x
	       	     sel c []  = c

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec c []						= ' '
lookUpRec c ((x,y) : xs)	| c == x		= y
 	    			| otherwise    		= lookUpRec c xs

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c xs = lookUp c xs == lookUpRec c xs

-- 5.
encipher :: Int -> Char -> Char
encipher n c = chr ((ord c) + n)

-- 6.
normalize :: String -> String
normalize []						= []
normalize (x:xs)	| isDigit x || isAlpha x	= toUpper x : normalize xs
	  		| otherwise    	       		= normalize xs

-- 7.
encipherStr :: Int -> String -> String
encipherStr n xs = normalize [encipher n x | x <- xs]

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey (xs:ys) = [(y,x) | (x,y) <- (xs:ys)]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec []			 	 = []
reverseKeyRec ((x,y) : xs)		  	 = (y,x) : reverseKeyRec xs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey = undefined
-- 9.
decipher :: Int -> Char -> Char
decipher n c = chr ((ord c) - n)

decipherStr :: Int -> String -> String
decipherStr n []					= []
decipherStr n (c:cs)	| isDigit c			= c : decipherStr n cs
	      		| isAlpha c && isUpper c	= decipher n c : decipherStr n cs
			| isAlpha c == False   		= c : decipherStr n cs
			| otherwise    			= decipherStr n cs

-- 10.
contains :: String -> String -> Bool
contains xs ys = and [elem x ys | x <- xs]

-- 11.
decipher26 :: String -> [String]
decipher26 cs = [decipherStr n cs | n <- [1..26]]

candidates :: String -> [(Int, String)]
candidates cs = [(n, decipherStr n cs) | n <- [0..25], contains "THE" (decipherStr n cs) || contains "AND" (decipherStr n cs)]

-- helper :: String -> [Int]
-- helper xs =  [decipherStr n xs | n <- [1..25]]


-- candidatesR :: String -> [(Int, String)]
-- candidatesR []					= []
-- candidatesR cs	| contains "THE" (helper cs)	= (n, helper cs) : candidates cs
-- 	      	| contains "AND" (helper cs) 	= (n, helper cs) : candidates cs
-- 		| otherwise 			= candidates cs


