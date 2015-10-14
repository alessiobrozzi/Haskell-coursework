-- Informatics 1 Functional Programming
-- December 2011

import Test.QuickCheck
import Control.Monad -- defines liftM, liftM2, used below

-- 1

-- 1a

f :: [Int] -> Int
f xs | null [ x | x <- xs , x>= 0] = 0
     | otherwise = maximum [ x | x <- xs, x >= 0 ]

test_f = f [1,2,3,4,5] == 5 && f [-1,2,-3,4,-5] == 4 && f [-1,-2,-3] == 0 && f [2,42,-7] == 42

-- 1b

g :: [Int] -> Int
g []                    = 0
g (x:xs)                = max x (g xs)

test_fg :: [Int] -> Bool
test_fg xs = f xs == g xs

check_fg = quickCheck test_fg

-- 2

-- 2a

p :: [Int] -> Int
p xs | even (length xs)   = sum [ x * y | (x,y) <- zip (f1 xs) (f2 xs) ]
     | otherwise          = error "not even length"
     where
        f1 []             = []
        f1 [x]            = [x]
        f1 (x:y:zs)       = x : f1 zs
        f2 []             = []
        f2 [x]            = []
        f2 (x:y:zs)       = y : f2 zs

test_p = p [1,2,3,4] == 14 && p [3,5,7,5,-2,4] == 42 && p [] == 0

-- 2b

q :: [Int] -> Int
q []                                    = 0
q [x]                                   = error "not even length"
q (x:y:zs)  | even (length (x:y:zs))    = (x * y) + q zs
            | otherwise                 = error "not even length"

test_q = q [1,2,3,4] == 14 && q [3,5,7,5,-2,4] == 42 && q [] == 0

test_pq :: [Int] -> Property
test_pq xs = even (length xs) ==> p xs == q xs

check_pq = quickCheck test_pq

check = check_fg >> check_pq

-- 2c

r :: [Int] -> Int
r (xs) = foldr (+) 0 (zipWith (*) (f1 xs) (f2 xs))
      where
        f1 []             = []
        f1 [x]            = [x]
        f1 (x:y:zs)       = x : f1 zs
        f2 []             = []
        f2 [x]            = []
        f2 (x:y:zs)       = y : f2 zs
r1 :: [Int] -> Int
r1 xs = foldr (+) 0 (map (\x -> (xs!!x) * (xs!!(x+1))) [0,2..(length xs - 1)])  

test_r1 = r1 [1,2,3,4] == 14 && r1 [3,5,7,5,-2,4] == 42 && r1 [] == 0

-- 3

data Expr = Var String
          | Expr :+: Expr
          | Expr :*: Expr
          deriving (Eq, Show)

-- code that enables QuickCheck to generate arbitrary values of type Expr

instance Arbitrary Expr where
  arbitrary = sized arb
    where
    arb 0          =  liftM Var arbitrary
    arb n | n > 0  =  oneof [liftM Var arbitrary,
                             liftM2 (:+:) sub sub, 
                             liftM2 (:*:) sub sub] 
      where
      sub = arb (n `div` 2)

-- 3a

isNorm :: Expr -> Bool
isNorm (Var x)                  = True
isNorm (p :*: q)                = isTerm p && isTerm q
isNorm (p :+: q)                = isNorm p && isNorm q

test_iN = isNorm (Var "x") == True && isNorm (Var "x" :*: Var "y" :*: Var "z") == True && isNorm ((Var "x" :*: Var "y") :+: Var "z") == True && isNorm (Var "x" :*: (Var "y" :+: Var "z")) == False && isNorm ((Var "x" :*: Var "y") :+: (Var "x" :*: Var "z")) == True && isNorm ((Var "u" :+: Var "v") :*: (Var "x" :+: Var "y")) == False && isNorm (((Var "u" :*: Var "x") :+: (Var "u" :*: Var "y")) :+: ((Var "v" :*: Var "x") :+: (Var "v" :*: Var "y"))) == True

isTerm :: Expr -> Bool
isTerm (Var x)                  = True
isTerm (p :*: q)                = isTerm p && isTerm q
isTerm (p :+: q)                = False

test_iT = isTerm (Var "x") == True && isTerm ((Var "x" :*: Var "y") :*: Var "z") == True && isTerm ((Var "x" :*: Var "y") :+: Var "z") == False && isTerm (Var "x" :*: (Var "y" :+: Var "z")) == False

-- 3b

norm :: Expr -> Expr
norm (Var x)                    = Var x
norm (Var x :*: Var y)          = Var x :*: Var y
norm ((p :+: q) :*: r)          = norm (p :*: r) :+: norm (q :*: r)
norm (p :*: (q :+: r))          = norm (p :*: q) :+: norm (p :*: r)
norm (p :*: q)                  = norm p :*: norm q
norm (p :+: q)                  = norm p :+: norm q

test_in = norm (Var "x") == (Var "x") && norm ((Var "x" :*: Var "y") :*: Var "z") == ((Var "x" :*: Var "y") :*: Var "z") && norm ((Var "x" :*: Var "y") :+: Var "z") == ((Var "x" :*: Var "y") :+: Var "z") && norm (Var "x" :*: (Var "y" :+: Var "z")) == ((Var "x" :*: Var "y") :+: (Var "x" :*: Var "z")) && norm ((Var "u" :+: Var "v") :*: (Var "x" :+: Var "y")) == (((Var "u" :*: Var "x") :+: (Var "u" :*: Var "y")) :+: ((Var "v" :*: Var "x") :+: (Var "v" :*: Var "y")))
