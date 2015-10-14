-- Informatics 1 - Functional Programming 
-- Tutorial 3
--
-- Week 5 - Due: 16/17 Oct.

import Data.Char
import Test.QuickCheck



-- 1. Map
-- a.
uppers :: String -> String
uppers str = map toUpper str

-- b.
help_double :: Int -> Int
help_double n = n * 2

doubles :: [Int] -> [Int]
doubles xs = map help_double xs

-- c.        
helper_pp :: Int -> Float
helper_pp n = fromIntegral (div (fromIntegral n) 100)

penceToPounds :: [Int] -> [Float]
penceToPounds ns = map helper_pp ns

-- d.
uppers' :: String -> String
uppers' cs = [ toUpper c | c <- cs ]

prop_uppers :: String -> Bool
prop_uppers cs = uppers cs == uppers' cs



-- 2. Filter
-- a.
alphas :: String -> String
alphas str = filter isAlpha str

-- b.
-- helper_rm returns False if the two input characters are equal, so that they can be removed from the list in rmChar
helper_rm :: Char -> Char -> Bool
helper_rm c d = if c == d then False else True

rmChar ::  Char -> String -> String
rmChar c str = filter (helper_rm c) str

-- c.
-- helper_ab returns False if x >= y
helper_ab :: Int -> Int -> Bool
helper_ab x y = if x >= y then False else True

above :: Int -> [Int] -> [Int]
above x ys = filter (helper_ab x) ys

-- d.
helper_ue :: (Int,Int) -> Bool
helper_ue (a,b) = if a == b then False else True

unequals :: [(Int,Int)] -> [(Int,Int)]
unequals ((x,y):ys) = filter (helper_ue) ys

-- e.
rmCharComp :: Char -> String -> String
rmCharComp a bs = [ b | b <- bs , helper_rm a b]

prop_rmChar :: Char -> String -> Bool
prop_rmChar a bs = rmChar a bs == rmCharComp a bs



-- 3. Comprehensions vs. map & filter
-- a.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

upperChars' :: String -> String
upperChars' cs = map toUpper (filter isAlpha cs)

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

-- helper_ld doubles each int n
helper_ld :: Int -> Int
helper_ld n = 2 * n

-- helper_ld2 tests if an int is greater than 3
helper_ld2 :: Int -> Bool
helper_ld2 n = if n > 3 then True else False

largeDoubles' :: [Int] -> [Int]
largeDoubles' xs = map helper_ld (filter helper_ld2 xs)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

-- helper_re tests if the length of a string is even
helper_re :: String -> Bool
helper_re str = even (length str)

reverseEven' :: [String] -> [String]
reverseEven' xs = map reverse (filter helper_re xs)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold xs = foldr (*) 1 xs

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.
bool_num :: Bool -> Int
bool_num x = if x == True then 1 else 0

andRec :: [Bool] -> Int
andRec []                               = 1
andRec (x:xs)	| bool_num x == 1	= (bool_num x) * andRec xs
       		| bool_num x == 0	= 0

-- andFold :: [Bool] -> Bool
-- andFold xs = foldr (and) True xs

-- prop_and :: [Bool] -> Bool
-- prop_and xs = andRec xs == andFold xs 

-- c.
concatRec :: [[a]] -> [a]
concatRec []				= []
concatRec [x:xs]			= x : concatRec [xs] 

concatFold :: [[a]] -> [a]
concatFold [x:xs] = foldr (++) [] [x:xs]

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.
rmCharsRec :: String -> String -> String
rmCharsRec [] xs                                 = []
rmCharsRec cs []                                 = []
rmCharsRec (c:cs) (x:xs)   | elem c xs == False  = rmCharsRec cs xs
                           | otherwise           = x : rmCharsRec cs xs     

rmCharsFold :: String -> String -> String
rmCharsFold cs xs = foldr rmChar xs cs

rCf :: String -> String -> String
rCf xs ys = foldr (\x ys -> rmChar x ys) xs ys

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str



type Matrix = [[Int]]


-- 5
-- a.
uniform :: [Int] -> Bool
uniform xs = and [x == head xs | x <- xs]

-- b.
valid :: Matrix -> Bool
valid = undefined

-- 6.

-- 7.
plusRow :: [Int] -> [Int] -> [Int]
plusRow [] ys            = ys
plusRow xs []            = xs
plusRow (x:xs) (y:ys)    = (x + y) : plusRow xs ys

-- plusM :: Matrix -> Matrix -> Matrix
-- plusM [xs] []          = [xs]
-- plusM [] [ys]          = [ys]
-- plusM [(x:xs)] [y:ys]  = plusRow x y : plusM [xs] [ys]

-- 8.
-- timesM :: Matrix -> Matrix -> Matrix
-- timesM (x:xs) ys | head (map length (x:xs) == head (map lenegth zs == map lengthdot x) zs :tM xs ys)
--                  | otherwise = error "uss...."
--                                  where zs = transpose ys
--                                        dot (x:xs) (y:ys) | le

-- Optional material
-- 9.

f :: Char -> Bool
f c = elem c ['a'..'m'] || elem c ['A'..'M']

g :: String -> Bool
g cs = length [c | c <- cs, f c] > length [c | c <- cs, f c == False]

c :: [Int] -> [Int]
c  xs = [x | x <- xs, x == head xs]


f2 :: Char -> Int
f2 x = if elem x "haskell" then 5 else if elem x "HASKELL" then 10 else if (elem x "haskell" == False) && isAlpha x && isLower x then 1 else if (elem x "HASKELL" == False) && isAlpha x && isUpper x then 2 else 0

g2 :: String -> Int
g2 cs = product [f2 c |c <- cs, isAlpha c ]

g3 :: String -> Int
g3 []                    = 1
g3 (c:cs)   | isAlpha c  = f2 c * g3 cs

com :: String -> String -> String
com xs ys = [x | (x,y) <- zip xs ys, x == y]

comRec :: String -> String -> String
comRec [] ys                    = []
comRec xs []                    = []
comRec (x:xs) (y:ys) | x == y   = x : comRec xs ys
                     | otherwise= comRec xs ys

prop_com :: String -> String -> Bool
prop_com xs ys = com xs ys == comRec xs ys


-- exam
c1 :: String -> String
c1 (x:xs) = [ toUpper x | (x,i) <- zip (x:xs) [0..], even i]

c2 :: String -> String
c2 (x:xs) = [ x | (x,i) <- zip (x:xs) [0..], not (even i)]

c3 :: String -> String
c3 xs = [ x:y | (x,y) <- zip (c1 xs) (c2 xs)]

d3 :: String -> String
d3 []                   = []
d3 (x:y:zs)             = (toUpper x):y: d3 zs
