-- rev 5
import Data.Char
import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>)  )

f1 :: [a] -> [a]
f1 xs = [ x | (x,i) <- zip xs [0..] , mod i 3 == 0 ]

g1 :: [a] -> [a]
g1 []                 = []
g1 (x:y:z:xs)         = x : g1 xs 
g1 small              = [head small]

h1 :: [a] -> [a]
h1 xs = map (\i -> xs !! i) [0,3..length xs - 1]

f2 :: [a] -> [a]
f2 xs = [ if even i then y else x | ((x,y),i) <- zip (help_f xs) [0..] ]
   where
        help_f xs = concat [ replicate 2 x | x <- help_f2 xs ]
               where 
                    help_f2 xs = zip [ x | (x,i) <- zip xs [0..] , even i ] [ x | (x,i) <- zip xs [0..] , odd i ]

g2 :: [a] -> [a]
g2 []                   = []
g2 [x]                  = [x]
g2 (x:y:zs)             = y:x: g2 zs

h2 :: [a] -> [a]
h2 xs = concat (map (\i -> [xs !! (i + 1), xs !! i]) [0,2..(length xs - 1)])

h3 :: [Int] -> Int
h3 xs = foldr (+) 0 (map (\i -> (xs !! i) * (xs !! (i + 1))) [0,2..(length xs -2)])