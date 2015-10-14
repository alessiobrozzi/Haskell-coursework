import Test.QuickCheck
import Data.Char

-- 1a.

inRange :: Int -> Bool
inRange x = x >= 0 && x <= 100

f :: [Int] -> Int
f xs = if null [ x | x <- xs , inRange x ] then 0 else maximum [ x | x <- xs , inRange x ]

test_f = f [40,50,60] == 60 && f [40,30,50,70,60] == 70 && f [-10,20,80,110] == 80 && f [-10,110] == 0 && f [] == 0

-- 1b.

g :: [Int] -> Int
g []                        = 0
g (x:xs) | inRange x        = max x (g xs)
         | otherwise        = g xs

-- 1c.

h :: [Int] -> Int
h xs = foldr max 0 (filter inRange xs)

test_fgh :: [Int] -> Bool
test_fgh xs = f xs == g xs && g xs == h xs

-- 2a.

p :: Int -> Int -> [Int]
p a b | a < b           = [ x | x <- [a..(b-1)] ]
      | otherwise       = reverse [ x | x <- [(b+1)..a] ]

test_p = p 3 7 == [3,4,5,6] && p 7 3 == [7,6,5,4]

-- 2b.

r :: [Int] -> [Int]
r xs = concat [ p (xs !! i) (xs !! (i+1)) | i <- [0..(length xs - 2)] ]

test_r = r [3,7,4,8] == [3,4,5,6,7,6,5,4,5,6,7] && r [1,3,5,3,5,7] == [1,2,3,4,5,4,3,4,5,6]

-- 2c.

s :: [Int] -> [Int]
s []                    = []
s [x]                   = []
s (x:y:zs)              = p x y ++ s (y:zs)

test_s = s [3,7,4,8] == [3,4,5,6,7,6,5,4,5,6,7] && s [1,3,5,3,5,7] == [1,2,3,4,5,4,3,4,5,6]

test_prs :: [Int] -> Bool
test_prs xs = r xs == s xs

check_prs = quickCheck test_prs

-- 3a.

t :: [Int] -> Int
t xs = sum [ 1 | (x,y) <- zip ([1]++xs) (xs++[1]), check x y ]
     where
        check x y =  x == 0 && y /= 0

-- 3b.

u :: [Int] -> Int
u xs = nonzero xs
  where
      nonzero (0:xs)    = 1 + zero xs
      nonzero (_:xs)    = nonzero xs
      nonzero []        = 0
      zero (0:xs)       = zero xs
      zero (_:xs)       = nonzero xs
      zero []           = 0
