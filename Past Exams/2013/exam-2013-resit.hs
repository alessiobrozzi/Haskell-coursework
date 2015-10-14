-- Informatics 1 Functional Programming
-- August 2014

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char

-- Question 1

-- 1a

f :: String -> String
f xs = concat [ replicate i x | (x,i) <- zip xs [1..] ]

test_f = f "abcde" == "abbcccddddeeeee" && f "ZYw" == "ZYYwww" && f "" == "" && f "Inf1FP" == "Innfff1111FFFFFPPPPPP"

-- 1b

g :: String -> String
g []                    = []
g (x:xs)                = g1 1 (x:xs)
  where
      g1 _ []     = []
      g1 n (x:xs) = replicate n x ++ g1 (n + 1) xs

test_fg :: String -> Bool
test_fg xs = f xs == g xs

check_fg = quickCheck test_fg


-- Question 2

-- 2a

p :: [String] -> Int
p xs = sum [ length x | x <- xs , elem '.' x ]

test_p = p ["Dr.","Who","crossed","the","ave."] == 7 && p ["the","sgt.","opened","the","encl.","on","Fri.","pm"] == 13 && p [] == 0 && p ["no","abbreviations","4U"] == 0

-- 2b

q :: [String] -> Int
q []                    = 0
q (x:xs) | elem '.' x   = length x + q xs
         | otherwise    = q xs

test_pq :: [String] -> Bool
test_pq xs = p xs == q xs

check_pq = quickCheck test_pq

-- 2c

r :: [String] -> Int
r xs = foldr (+) 0 (map length (filter (elem '.') xs))

test_pqr :: [String] -> Bool
test_pqr xs = p xs == q xs && q xs == r xs

check_pqr = quickCheck test_pqr

-- Question 3

data Tree = Empty
          | Leaf Int
          | Node Tree Tree
        deriving (Eq, Ord, Show)

-- For QuickCheck

instance Arbitrary Tree where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  oneof [elements [Empty]]
                 | otherwise  =  oneof [ liftM Leaf arbitrary
                                       , liftM2 Node subform subform
                                       ]
                 where
                   subform  =  expr (n `div` 2)

-- For testing

t1 = Empty

t2 = Node (Leaf 1)
          Empty

t3 = Node (Node (Node (Leaf 3)
                      Empty)
                (Leaf 1))
          (Node Empty
                (Node (Leaf 3)
                      (Leaf 5)))

t4 = Node (Node (Node Empty
                      Empty)
                (Leaf 1))
          (Node Empty
                (Node Empty
                      Empty))

-- 3a

leafdepth :: Tree -> Int
leafdepth Empty              = 0
leafdepth (Leaf x)           = 1
leafdepth (Node x y) | leafdepth x == 0 && leafdepth y == 0 = 0
                     | otherwise                            = 1 + ld x y
      where 
          ld x y | leafdepth x > leafdepth y = leafdepth x
                 | otherwise                 = leafdepth y

-- 3 b

helper :: Int -> Tree -> [Int]
helper 0 _            = []
helper _ Empty        = []
helper 1 (Leaf x)     = [x]
helper n (Leaf _)     = []
helper n (Node x y)   = helper (n-1) x ++ helper (n-1) y

deepest1 :: Tree -> [Int]
deepest1 t = helper (leafdepth t) t

-- 3c

deepest2 :: Tree -> [Int]
deepest2 Empty                                          = []
deepest2 (Node x y) | leafdepth x == leafdepth y        = deepest2 x ++ deepest2 y
                    | leafdepth x > leafdepth y         = deepest2 x
                    | otherwise                         = deepest2 y
deepest2 (Leaf x)                                       = [x]

prop3 :: Tree -> Bool
prop3 x = deepest1 x == deepest2 x

check3 = quickCheck prop3