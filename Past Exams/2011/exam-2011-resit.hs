-- Informatics 1 Functional Programming
-- Resit, August 2012

import Data.Char
import Test.QuickCheck
import Control.Monad -- defines liftM, liftM2, used below

-- 1

-- 1a

f :: [String] -> String
f xs = concat [ x | x <- xs , null x || isUpper (head x) ]

test_f = f ["This","Is","not","A","non","Test"] == "ThisIsATest" && f ["noThing","beGins","uPPER"] == "" && f ["Non-words","like","42","get","Dropped"] == "Non-wordsDropped" && f ["An","Empty","Word","","gets","dropped"] == "AnEmptyWord"

-- 1b

g :: [String] -> String
g []                                          = ""
g (x:xs) | null x || isUpper (head x)         = x ++ g xs
         | otherwise                          = g xs

test_fg :: [String] -> Bool
test_fg xs = f xs == g xs

check_fg = quickCheck test_fg

-- 2

-- 2a

p :: [(Int,Int)] -> Bool
p (x:xs) | null (x:xs)   = error "empty list"
         | otherwise     = and [ b == c | ((a,b),(c,d)) <- zip (x:xs) xs ]

test_r = r [(1,2),(2,3),(3,4)] == True && r [(9,5),(5,5),(5,7),(7,-2)] == True && p [(1,2),(3,4)] == False && r [(1,2),(2,3),(33,4)] == False && r [] == error "empty list"

-- 2b

q :: [(Int,Int)] -> Bool
q []               = error " "
q [x]              = True
q (x:y:xs)  = check1 x y && q (y:xs)
  where
      check1 (a,b) (c,d) = b == c
-- 2c

r :: [(Int,Int)] -> Bool
r xs   | null xs = error " "
       | otherwise = foldr (&&) True (map (\x -> check1 x) [0..(length xs - 2)])
                         where
                            check1 i = snd (xs !! i) == fst (xs !! (i + 1))


-- 3

data Expr = Var String
          | Const Int
          | Expr :+: Expr
          | Expr :*: Expr
          deriving (Eq, Show)

-- code that enables QuickCheck to generate arbitrary values of type Expr

instance Arbitrary Expr where
  arbitrary = sized arb
    where
    arb 0          =  liftM Var arbitrary
    arb n | n > 0  =  oneof [liftM Const arbitrary,
                             liftM Var arbitrary,
                             liftM2 (:+:) sub sub, 
                             liftM2 (:*:) sub sub] 
      where
      sub = arb (n `div` 2)

-- 3a

isSimple :: Expr -> Bool
isSimple (Const _)                      = True
isSimple (Var _)                        = True
isSimple (p :+: q)                      = cases p q
      where
        cases (Const _) (Const _)       = False
        cases (Const 0) _               = False
        cases _ (Const 0)               = False
        cases p q                       = isSimple p && isSimple q
isSimple (p :*: q)                      = cases p q
      where
        cases (Const _) (Const _)       = False
        cases (Const 0) _               = False
        cases _ (Const 0)               = False
        cases p q                       = isSimple p && isSimple q

test_is = isSimple (Var "x" :+: Var "y") == True && isSimple (Const 1 :+: Const 2) == False && isSimple (Const 0 :+: Var "x") == False && isSimple ((Var "y" :+: Const 1) :*: Var "x") == True && isSimple (Var "y" :+: (Var "x" :*: Const 0)) == False && isSimple (Var "x" :*: (Const 3 :+: Const (-2))) == False

-- 3b

simplify :: Expr -> Expr
simplify (Var x)                        = Var x
simplify (Const x)                      = Const x
simplify (p :*: q)                      = reduce (simplify p) (simplify q)
      where
        reduce (Const x) (Const y)      = Const (x * y)
        reduce (Const 0) _              = Const 0
        reduce _ (Const 0)              = Const 0
        reduce (Const 1) x              = x
        reduce x (Const 1)              = x
        reduce p q                      = p :*: q
simplify (p :+: q)                      = reduce (simplify p) (simplify q)
      where
        reduce (Const x) (Const y)      = Const (x + y)
        reduce (Const 0) x              = x
        reduce x (Const 0)              = x
        reduce p q                      = p :+: q

test_s = simplify (Var "x" :+: Var "y") ==  (Var "x" :+: Var "y") && simplify (Const 1 :+: Const 2) == Const 3 && simplify (Const 0 :+: Var "x") == Var "x" && simplify ((Var "y" :+: Const 1) :*: Var "x") == ((Var "y" :+: Const 1) :*: Var "x") && simplify (Var "y" :+: (Var "x" :*: Const 0)) == Var "y" && simplify (Var "x" :*: (Const 3 :+: Const (-2))) == Var "x"

check_iss :: Expr -> Bool
check_iss x = isSimple (simplify x)
