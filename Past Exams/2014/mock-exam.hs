-- Mock Exam

import Data.Char
import Data.List
import Control.Monad
import Test.QuickCheck



-- 1

isCard :: Char -> Bool
isCard x = x >= '2' || x <= '9' || x == '0' || x == 'A' || x == 'K' || x == 'Q' || x == 'J'

isFace :: Char -> Bool
isFace x = isDigit x && isCard x

f :: String -> Bool
f xs = and [ isFace x | x <- xs , isCard x ]

g :: String -> Bool
g []                    = True
g (x:xs) | isCard x     = isFace x && g xs
         | otherwise    = g xs

h :: String -> Bool
h xs = foldr (&&) True (map isFace (filter isCard xs))

test_fg :: String -> Bool
test_fg xs = f xs == g xs && g xs == h xs

check_fg = quickCheck test_fg

-- 2

t :: [a] -> [a]
t xs = concat [ if even i then [x] else (replicate 2 x) | (x,i) <- zip xs [0..] ]

test_t = t "abcdefg" == "abbcddeffg" && t [1,2,3,4] == [1,2,2,3,4,4] && t "" == ""

u :: [a] -> [a]
u []                  = []
u [x]                 = [x]
u (x:y:zs)            = x : y : y : u zs

test_tu :: (Eq a) => [a] -> Bool
test_tu xs = t xs == u xs

-- 3

data Proposition = Var String
                 | F
                 | T
                 | Not Proposition
                 | Proposition :|: Proposition
                 | Proposition :&: Proposition
                 deriving (Eq, Ord, Show)

instance Arbitrary Proposition where
  arbitrary = sized expr
    where
      expr 0 =
        oneof [return F,
               return T,
               liftM Var (elements ["p", "q", "r", "s", "t"])]
      expr n | n > 0 =
        oneof [return F,
               return T,
               liftM Var (elements ["p", "q", "r", "s", "t"]),
               liftM Not (expr (n-1)),
               liftM2 (:&:) (expr (n `div` 2)) (expr (n `div` 2)),
               liftM2 (:|:) (expr (n `div` 2)) (expr (n `div` 2))]

isNorm :: Proposition -> Bool
isNorm (Var _)                          = True
isNorm T                                = True
isNorm F                                = True
isNorm (Not (Var _))                    = True
isNorm (Not _)                          = False
isNorm (p :|: q)                        = isNorm p && isNorm q
isNorm (p :&: q)                        = isNorm p && isNorm q

test_in = isNorm (Var "p" :&: Not (Var "q")) == True && isNorm (Not (Var "p" :|: Var "q")) == False && isNorm (Not (Not (Var "p")) :|: Not T) == False && isNorm (Not (Var "p" :&: Not (Var "q"))) == False

norm :: Proposition -> Proposition
norm T                                  = T
norm F                                  = F
norm (Var x)                            = Var x
norm (Not p)                            = negate (norm p)
     where
        negate T                        = F
        negate F                        = T
        negate (Var x)                  = Not (Var x)
        negate (Not x)                  = x
        negate (p :|: q)                = negate p :&: negate q
        negate (p :&: q)                = negate p :|: negate q
norm (p :&: q)                          = norm p :&: norm q
norm (p :|: q)                          = norm p :|: norm q

test_n = norm (Var "p" :&: Not (Var "q")) == (Var "p" :&: Not (Var "q")) && norm (Not (Var "p" :|: Var "q")) == Not (Var "p") :&: Not (Var "q") && norm (Not (Not (Var "p")) :|: Not T) == (Var "p" :|: F) && norm (Not (Var "p" :&: Not (Var "q"))) == Not (Var "p") :|: Var "q"

test :: Proposition -> Bool
test p = isNorm (norm p)
