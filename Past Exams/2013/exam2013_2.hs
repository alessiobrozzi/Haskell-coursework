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
f xs = base (reverse xs)
     where
        base xs = sum [ (digitToInt x) * (4 ^ i) | (x,i) <- zip xs [0..] ]

-- 1b

base2 :: Char -> Int -> Int
base2 a b = (digitToInt a) * (4 ^ b)

g :: String -> Int
g xs = help (reverse xs) [0..]
     where
        help [] (a:as)       = 0
        help (x:xs) (a:as)   = ((digitToInt x) * (4 ^ a)) + help xs as


-- Question 2

-- 2a

check :: Int -> Int -> Bool
check b a = a >= ( 2 * b)

p :: [Int] -> Bool
p (x:xs) | null (x:xs)   = error "empty list" 
         | otherwise     = and [ check y x | (x,y) <- zip xs (replicate (length (x:xs)) (head (x:xs))) , x > 0 ]

-- 2b

q :: [Int] -> Bool
q []    = True
q xs    = control xs
          where
                control []                      = True
                control [x]                     = True
                control (x:y:xs) | y > 0        = check y x && control (x:xs)
                                 | otherwise    = control (x:xs)

-- 2c

r :: [Int] -> Bool
r xs = foldr (&&) True (t xs (head xs))
       where
            t (x:xs) a = map (>= (2 * a)) (filter (>0) xs)

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
ppn (p :+: q)       = ["+"] ++ ppn p ++ ppn q
ppn (p :*: q)       = ["*"] ++ ppn p ++ ppn q
ppn (Neg p)         = ["-"] ++ ppn p
ppn X               = ["X"]
ppn (Const x)       = [show x]

-- 3 b

evalppn :: [String] -> Int -> Int
evalppn (x:xs) n | x == "X"                     = evalExpr (Const n : evalppn xs n)
                 | read x :: Int == Const x     = evalExpr (Const x : evalppn xs n)
                 | x == "+"                     = evalExpr (
