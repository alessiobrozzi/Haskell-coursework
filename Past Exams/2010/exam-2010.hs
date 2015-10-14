import Test.QuickCheck

-- 1a

f :: [Int] -> Int
f xs = product [ round ((fromIntegral x) / 2) | x <- xs , even x ]

test_f = f [1,2,3,4,5,6] == 6 && f [2,4,6,8] == 24 && f [4,-4,4] == -8 && f [2,2,2] == 1 && f [1,3,5] == 1

-- 1b

g :: [Int] -> Int
g []                    = 1
g (x:xs) | even x       = g1 x * g xs
         | otherwise    = g xs
   where
       g1 x = round ((fromIntegral x) /2)

-- 1c

h :: [Int] -> Int
h xs = foldr (*) 1 (map h1 (filter even xs))
  where
      h1 x = round ((fromIntegral x) / 2)

test_fgh :: [Int] -> Bool
test_fgh xs = f xs == g xs && g xs == h xs

check_fgh = quickCheck test_fgh

-- 2a

p2 :: [a] -> [a]
p2 xs = concat [ [xs!!(i+1)] ++ [xs!!i] | i <- [0,2..(length xs -1)] ]

p :: [a] -> [a]
p xs = [ if even i then b else a | ((a,b),i) <- zip (p1 xs) [0..] ]
     where
        p1 xs = zip (p2 xs) (p3 xs)
           where
              p2 xs = concat [ replicate 2 x | (x,i) <- zip xs [0..] , even i ]
              p3 xs = concat [ replicate 2 x | (x,i) <- zip xs [0..] , odd i ]

-- 2b

q :: [a] -> [a]
q []            = []
q [x]           = [x]
q (x:y:zs)      = y : x : q zs

test_pq :: [Int] -> Property
test_pq xs = even (length xs) ==> p xs == q xs

check_pq = quickCheck test_pq

check = check_fgh >> check_pq

-- 3a

type Scalar = Int
type Vector = (Int,Int)

add :: Vector -> Vector -> Vector
add = undefined

mul :: Scalar -> Vector -> Vector
mul = undefined

-- 3b

data Term  =  Vec Scalar Scalar
            | Add Term Term
            | Mul Scalar Term

eva :: Term -> Vector
eva = undefined

-- 3c

sho :: Term -> String
sho = undefined

