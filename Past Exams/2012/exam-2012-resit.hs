-- Informatics 1 Functional Programming
-- August 2013

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )
import Control.Monad -- defines liftM, liftM2, used below

-- Question 1

-- 1a

f :: [(Int,Int)] -> [Int]
f xs = [ if even i then a else b | ((a,b),i) <- zip xs [0..] ]

test_f = f [(1,2),(5,7),(3,8),(4,9)] == [1,7,3,9] && f [(1,2)] == [1] && f [] == []

-- 1b

g :: [(Int,Int)] -> [Int]
g []             = []
g [x]            = [takefirst x]
        where
            takefirst (a,b) = a
g (x:y:zs)       = takefirst x : takesecond y : g zs
        where
            takefirst (a,b) = a
            takesecond (a,b) = b

test_g = g [(1,2),(5,7),(3,8),(4,9)] == [1,7,3,9] && g [(1,2)] == [1] && g [] == []

-- Question 2

-- 2a

p :: [Int] -> Int
p xs = product [ x * 3 | x <- xs, posodd x ]
     where
        posodd x = x >= 0 && not (even x)

test_p = p [1,6,-15,11,-9] == 99 && p [3,6,9,12,-9,9] == 6561 && p [] == 1 && p [-1,4,-15] == 1

-- 2b

q :: [Int] -> Int
q []                    = 1
q (x:xs) | posodd x     = (3 * x) * q xs
         | otherwise    = q xs
     where
        posodd x = x >= 0 && not (even x)

test_q = q [1,6,-15,11,-9] == 99 && q [3,6,9,12,-9,9] == 6561 && q [] == 1 && q [-1,4,-15] == 1

-- 2c

r :: [Int] -> Int
r xs = foldr (*) 1 (map (* 3) (filter posodd xs))
     where
        posodd x = x >= 0 && not (even x)

test_r = r [1,6,-15,11,-9] == 99 && r [3,6,9,12,-9,9] == 6561 && r [] == 1 && r [-1,4,-15] == 1

-- Question 3

data Prop = X
          | F
          | T
          | Not Prop
          | Prop :<->: Prop
          deriving (Eq, Ord)

-- turns a Prop into a string approximating mathematical notation

showProp :: Prop -> String
showProp X            =  "X"
showProp F            =  "F"
showProp T            =  "T"
showProp (Not p)      =  "(~" ++ showProp p ++ ")"
showProp (p :<->: q)  =  "(" ++ showProp p ++ "<->" ++ showProp q ++ ")"

-- For QuickCheck

instance Show Prop where
    show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:<->:) subform subform
                                       ]
                 where
                   atom = oneof [elements [X,F,T]]
                   subform  =  prop (n `div` 2)

-- 3a

eval :: Prop -> Bool -> Bool
eval T _                        = True
eval F _                        = False
eval X x                        = x
eval (Not p) x                  = not (eval p x)
eval (p :<->: q) x              = coimplication (eval p x) (eval q x)
        where
           coimplication a b = ((not a) || b) && ((not b) || a)

test_eval = eval (Not T) True == False && eval (Not X) False == True && eval (Not X :<->: Not (Not X)) True == False && eval (Not X :<->: Not (Not X)) False == False && eval (Not (Not X :<->: F)) True == False && eval (Not (Not X :<->: F)) False == True

-- 3b

simplify :: Prop -> Prop
simplify X                              = X
simplify T                              = T
simplify F                              = F
simplify (Not p)                        = recur (simplify p)
         where
            recur T                     = F
            recur F                     = T
            recur X                     = Not X
            recur (Not X)               = X
            recur p                     = Not (simplify p)
simplify (p :<->: q)                    = recurs (simplify p) (simplify q)
         where
            recurs T p                  = p
            recurs F p                  = Not p
            recurs p T                  = p
            recurs p F                  = Not p
            recurs p q | p == q         = T
                       | otherwise      = p :<->: q

test :: Prop -> Bool -> Bool
test x a = eval x a == eval (simplify x) a
