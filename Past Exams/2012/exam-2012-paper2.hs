-- Informatics 1 Functional Programming
-- December 2012
-- SITTING 2 (14:30 - 16:30)

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )
import Control.Monad -- defines liftM, liftM2, used below

-- Question 1

-- 1a

f :: Char -> String -> String
f a xs = [ if even i then a else x | (x,i) <- zip xs [0..] ]

test_f = f '.' "abcdefg" == ".b.d.f." && f '.' "abcd" == ".b.d" && f '.' "" == [] && f '.' "a" == "."

-- 1b

g :: Char -> String -> String
g _ []              = []
g a [x]             = [a]
g a (x:y:zs)        = a : y : g a zs

test_fg :: Char -> String -> Bool
test_fg a xs = f a xs == g a xs

check_fg = quickCheck test_fg

-- Question 2

-- 2a

p :: [Int] -> Bool
p xs = and [ mod x 3 == 0 | x <- xs , x >= 0 ]

-- 2b

q :: [Int] -> Bool
q []                   = True
q (x:xs) | x > 0       = mod x 3 == 0 && q xs
         | otherwise   = q xs

-- 2c

r :: [Int] -> Bool
r xs = foldr (&&) True (map div3 (filter ( >= 0 ) xs))
  where
      div3 a = mod a 3 == 0

test_pqr :: [Int] -> Bool
test_pqr xs = p xs == q xs && q xs == r xs

check_pqr = quickCheck test_pqr

-- Question 3

data Prop = X
          | F
          | T
          | Not Prop
          | Prop :&: Prop
          deriving (Eq, Ord)

-- turns a Prop into a string approximating mathematical notation

showProp :: Prop -> String
showProp X           =  "X"
showProp F           =  "F"
showProp T           =  "T"
showProp (Not p)     =  "(~" ++ showProp p ++ ")"
showProp (p :&: q)   =  "(" ++ showProp p ++ "&" ++ showProp q ++ ")"

-- For QuickCheck

instance Show Prop where
    show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:&:) subform subform
                                       ]
                 where
                   atom = oneof [elements [X,F,T]]
                   subform  =  prop (n `div` 2)

-- 3a

eval :: Prop -> Bool -> Bool
eval F _                                = False
eval T _                                = True
eval X x                                = x
eval (Not p) x                          = not (eval p x)
eval (p :&: q) x                        = eval p x && eval q x

test_eval = eval (Not F) True == True && eval (Not X) False == True && eval (Not X :&: Not (Not X)) True == False && eval (Not X :&: Not (Not X)) False == False && eval (Not (Not X :&: T)) True == True && eval (Not (Not X :&: T)) False == False

-- 3b

simplify :: Prop -> Prop
simplify F                              = F
simplify T                              = T
simplify X                              = X
simplify (Not p)                        = negate (simplify p)
         where
             negate F                   = T
             negate T                   = F
             negate (Not x)             = x
             negate x                   = Not x
simplify (p :&: q) | simplify p == T    = simplify q
                   | simplify p == F    = F
                   | simplify q == T    = simplify p    
                   | simplify q == F    = F
                   | simplify p == simplify q = simplify p
                   | otherwise          = simplify p :&: simplify q

test_simp = simplify (Not X :&: Not (Not X)) == Not X :&: X && simplify (Not (Not X :&: F)) == T && simplify (Not T) == F && simplify (Not F :&: X) == X && simplify (Not (Not (Not X) :&: X)) == Not X

test_es e = eval e True == eval (simplify e) True && eval e False == eval (simplify e) False