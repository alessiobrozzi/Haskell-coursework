-- Informatics 1 Functional Programming
-- December 2013
-- SITTING 2 (14:30 - 16:30)

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>)  )
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char

-- Question 1

-- 1a

f :: String -> Int
f = undefined

-- 1b

g :: String -> Int
g = undefined


-- Question 2

-- 2a
muchBigger :: Int -> Int -> Bool
muchBigger a b = a >= 2 * b

p :: [Int] -> Bool
p (a:xs) = and [ x `muchBigger` a | x <- xs, x >= 0 ]

test_p = p [2,6,-3,18,-7,10] == True && p [13] == True && p [-3,6,1,-6,9,18] == True && p [5,-2,-6,7] == False

-- 2b

q :: [Int] -> Bool
q []                    = True
q (x:y:xs) | x > 0      = y >= 2 * x && q (x:xs)
           | otherwise  = q (x:xs)

-- 2c

r :: [Int] -> Bool
r (x:xs) = foldr (&&) True (map (>= 2 * x) (filter ( > 0 ) xs))

test_pqr :: [Int] -> Bool
test_pqr xs = p xs == q xs && q xs == r xs

check_pqr = quickCheck test_pqr

-- Question 3

data Expr = X
          | Const Int
          | Neg Expr
          | Expr :+: Expr
          | Expr :*: Expr
          deriving (Eq, Ord)

-- turns an Expr into a string approximating mathematical notation

showExpr :: Expr -> String
showExpr X          =  "X"
showExpr (Const n)  =  show n
showExpr (Neg p)    =  "(-" ++ showExpr p ++ ")"
showExpr (p :+: q)  =  "(" ++ showExpr p ++ "+" ++ showExpr q ++ ")"
showExpr (p :*: q)  =  "(" ++ showExpr p ++ "*" ++ showExpr q ++ ")"

-- evaluate an Expr, given a value of X

evalExpr :: Expr -> Int -> Int
evalExpr X v          =  v
evalExpr (Const n) _  =  n
evalExpr (Neg p) v    =  - (evalExpr p v)
evalExpr (p :+: q) v  =  (evalExpr p v) + (evalExpr q v)
evalExpr (p :*: q) v  =  (evalExpr p v) * (evalExpr q v)

-- For QuickCheck

instance Show Expr where
    show  =  showExpr

instance Arbitrary Expr where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  oneof [elements [X]]
                 | otherwise  =  oneof [ liftM Const arbitrary
                                       , liftM Neg subform
                                       , liftM2 (:+:) subform subform
                                       , liftM2 (:*:) subform subform
                                       ]
                 where
                   subform  =  expr (n `div` 2)

-- 3a

ppn :: Expr -> [String]
ppn X                           = ["X"]
ppn (Const x)                   = [show x]
ppn (Neg x)                     = ["-"] ++ ppn x
ppn (p :*: q)                   = ["*"] ++ ppn p ++ ppn q
ppn (p :+: q)                   = ["+"] ++ ppn p ++ ppn q

-- 3 b

evalppn :: [String] -> Int -> Int
evalppn = undefined
