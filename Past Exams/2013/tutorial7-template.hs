import Test.QuickCheck
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char

-- Question 1

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

-- 1a

isNorm :: Expr -> Bool
isNorm (Var x)          = True
isNorm (p :*: q)        = isTerm (p :*: q)
isNorm (p :+: q)        = isNorm p && isNorm q

isTerm :: Expr -> Bool
isTerm (Var x)          = True
isTerm (p :*: q)        = isTerm p && isTerm q
isTerm (p :+: q)        = False

test_it = isTerm (Var "x") == True && isTerm ((Var "x" :*: Var "y") :*: Var "z") == True &&isTerm ((Var "x" :*: Var "y") :+: Var "z") == False && isTerm (Var "x" :*: (Var "y" :+: Var "z")) == False

test_in = isNorm (Var "x") == True && isNorm (Var "x" :*: Var "y" :*: Var "z") == True && isNorm ((Var "x" :*: Var "y") :+: Var "z") == True && isNorm (Var "x" :*: (Var "y" :+: Var "z")) == False && isNorm ((Var "x" :*: Var "y") :+: (Var "x" :*: Var "z")) == True && isNorm ((Var "u" :+: Var "v") :*: (Var "x" :+: Var "y")) == False && isNorm (((Var "u" :*: Var "x") :+: (Var "u" :*: Var "y")) :+: ((Var "v" :*: Var "x") :+: (Var "v" :*: Var "y"))) == True

-- 1b

norm :: Expr -> Expr
norm (Var x)                    = Var x
norm (p :+: q)                  = norm p :+: norm q
norm (p :*: q)                  = normalize (norm p) (norm q)
     where
        normalize (a :+: b) c   = normalize a c :+: normalize b c
        normalize a (b :+: c)   = normalize a b :+: normalize a c
        normalize a b           = a :*: b

test_n = norm (Var "x") == (Var "x") && norm ((Var "x" :*: Var "y") :*: Var "z") == ((Var "x" :*: Var "y") :*: Var "z") && norm ((Var "x" :*: Var "y") :+: Var "z") == ((Var "x" :*: Var "y") :+: Var "z") && norm (Var "x" :*: (Var "y" :+: Var "z")) == ((Var "x" :*: Var "y") :+: (Var "x" :*: Var "z")) && norm ((Var "u" :+: Var "v") :*: (Var "x" :+: Var "y")) == (((Var "u" :*: Var "x") :+: (Var "u" :*: Var "y")) :+: ((Var "v" :*: Var "x") :+: (Var "v" :*: Var "y")))

test_inn :: Expr -> Bool
test_inn x = isNorm (norm x)

-- Question 2

-- 2a

type Scalar = Int
type Vector = (Int,Int)

add :: Vector -> Vector -> Vector
add (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

mul :: Scalar -> Vector -> Vector
mul n (a,b) = (a*n,b*n)

-- 2b

data Term  =  Vec Scalar Scalar
            | Add Term Term
            | Mul Scalar Term
          deriving (Eq, Show)

-- code that enables QuickCheck to generate arbitrary values of type Term

instance Arbitrary Term where
  arbitrary = sized arb
    where
    arb 0          =  liftM2 Vec arbitrary arbitrary
    arb n | n > 0  =  oneof [liftM2 Vec arbitrary arbitrary,
                             liftM2 Add sub sub, 
                             liftM2 Mul arbitrary sub] 
      where
      sub = arb (n `div` 2)

eva :: Term -> Vector
eva (Vec a b)                 = (a,b)
eva (Add x y)                 = add (eva x) (eva y)
eva (Mul x y)                 = mul x (eva y)

test_eva = eva (Vec 1 2) == (1,2) && eva (Add (Vec 1 2) (Vec 3 4)) == (4,6) && eva (Mul 2 (Vec 3 4)) == (6,8) && eva (Mul 2 (Add (Vec 1 2) (Vec 3 4))) == (8,12) && eva (Add (Mul 2 (Vec 1 2)) (Mul 2 (Vec 3 4))) == (8,12)

-- 2c

sho :: Term -> String
sho (Vec a b)                 = "(" ++ show a ++ "," ++ show b ++ ")"
sho (Add x y)                 = "(" ++ sho x ++ "+" ++ sho y ++ ")"
sho (Mul x y)                 = "(" ++ show x ++ "*" ++ sho y ++ ")"

test_sho = sho (Vec 1 2) == "(1,2)" && sho (Add (Vec 1 2) (Vec 3 4)) == "((1,2)+(3,4))" && sho (Mul 2 (Vec 3 4)) == "(2*(3,4))" && sho (Mul 2 (Add (Vec 1 2) (Vec 3 4))) == "(2*((1,2)+(3,4)))" && sho (Add (Mul 2 (Vec 1 2)) (Mul 2 (Vec 3 4))) == "((2*(1,2))+(2*(3,4)))"

-- Question 3

type Point = (Int,Int)
data Points = Rectangle Point Point
            | Union Points Points
            | Difference Points Points
          deriving (Eq, Show)

-- code that enables QuickCheck to generate arbitrary values of type Points

instance Arbitrary Points where
  arbitrary = sized arb
    where
    arb 0          =  liftM2 Rectangle arbitrary arbitrary
    arb n | n > 0  =  oneof [liftM2 Rectangle arbitrary arbitrary,
                             liftM2 Union sub sub, 
                             liftM2 Difference sub sub] 
      where
      sub = arb (n `div` 2)

-- 3a

inRange :: Int -> Int -> Int -> Bool
inRange x a b = x >= a && x <= b

inPoints :: Point -> Points -> Bool
inPoints (x,y) (Rectangle (a,b) (c,d))     = inRange x a c && inRange y b d
inPoints x (Union a b)                     = inPoints x a || inPoints x b
inPoints x (Difference a b)                = inPoints x a && not (inPoints x b)

test_iP = inPoints (1,1) (Rectangle (0,0) (2,1)) == True && inPoints (3,4) (Rectangle (0,0) (2,1)) == False && inPoints (1,1) (Union (Rectangle (0,0) (0,1)) (Rectangle (1,0) (1,1))) == True && inPoints (2,2) (Union (Rectangle (0,0) (0,1)) (Rectangle (1,0) (1,1))) == False && inPoints (1,1) (Difference (Rectangle (0,0) (1,1)) (Rectangle (0,0) (0,1))) == True && inPoints (0,0) (Difference (Rectangle (0,0) (1,1)) (Rectangle (0,0) (0,1))) == False

-- 3b

showPoints :: Point -> Points -> [String]
showPoints = undefined

-- rev 8

f28 :: [Int] -> [Int] -> Int
f28 xs ys | length xs /= length ys = error "dif length"
          | otherwise              = product [ x | (x,y) <- zip xs ys , mod x y == 0 ]

f38 :: String -> [String]
f38 ""                  = []
f38 (x:xs) | isUpper x  = [x] : f38bis xs : f38 xs
           | otherwise  = f38 xs
    where
        f38bis []                       = []
        f38bis (x:xs) | isLower x       = x : f38bis xs
                      | otherwise       = []

f48 :: [Int] -> [Int] -> [Int]
f48 xs ys = [ x + y | x <- xs , y <- ys ]

f48h :: [Int] -> [Int] -> [Int]
f48h [] ys     = []
f48h xs []     = []
f48h xs (y:ys) = map (\x -> x + y) xs ++ f48h xs ys