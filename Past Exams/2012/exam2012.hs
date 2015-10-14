-- December 2012
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )
import Control.Monad -- defines liftM, liftM2, used below

-- Question 1

-- 1a

f :: Int -> [Int] -> [Int]
f n xs = [ if (even i) then n else x | (x,i) <- zip xs [0..]]

-- 1b

g :: Int -> [Int] -> [Int]
g n []            = []
g n [x]           = [n]
g n (x:y:zs)      = n : y : g n zs

-- Question 2

-- 2a

p :: [Int] -> Bool
p xs = and [ even x | x <- xs , x >= 10 && x <= 100 ]

-- 2b

q :: [Int] -> Bool
q []                               = True
q (x:xs)  | x >= 10 && x<=100      = even x && q xs
          | otherwise              = q xs

-- 2c

r :: [Int] -> Bool
r xs = foldr (&&) True (map even (filter help xs))
     where
        help x = x >= 10 && x <= 100

-- Question 3

data Prop = X
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          deriving (Eq, Ord)

-- turns a Prop into a string approximating mathematical notation

showProp :: Prop -> String
showProp X          =  "X"
showProp F          =  "F"
showProp T          =  "T"
showProp (Not p)    =  "(~" ++ showProp p ++ ")"
showProp (p :|: q)  =  "(" ++ showProp p ++ "|" ++ showProp q ++ ")"

-- For QuickCheck

instance Show Prop where
    show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:|:) subform subform
                                       ]
                 where
                   atom = oneof [elements [X,F,T]]
                   subform  =  prop (n `div` 2)

-- 3a

eval :: Prop -> Bool -> Bool
eval F _                        = False
eval T _                        = True
eval X v                        = v
eval (Not T) _                  = False
eval (Not F) _                  = True
eval (Not p) x                  = not (eval p x)
eval (p :|: q) x                = eval p x || eval q x

-- 3b

simplify :: Prop -> Prop
simplify X                      = X
simplify T                      = T
simplify F                      = F
simplify (Not p)                = notting (simplify p)
         where
            notting T       = F
            notting F       = T
            notting (Not p) = p
            notting p       = Not p
simplify (p :|: q)              = red (simplify p) (simplify q)
         where
            red T p         = T
            red F p         = p
            red p T         = T
            red p F         = p
            red p q | p == q = p
                    | otherwise = p :|: q
