-- Informatics 1 - Functional Programming 
-- Tutorial 8
--
-- Week 11 - due: 28/29 Nov.

import Data.List
import Test.QuickCheck



-- Type declarations

type FSM q = ([q], Alphabet, q, [q], [Transition q])
type Alphabet = [Char]
type Transition q = (q, Char, q)



-- Example machines

m1 :: FSM Int
m1 = ([0,1,2,3,4],
      ['a','b'],
      0,
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])

m2 :: FSM Char
m2 = (['A','B','C','D'],
      ['0','1'],
      'B',
      ['A','B','C'],
      [('A', '0', 'D'), ('A', '1', 'B'),
       ('B', '0', 'A'), ('B', '1', 'C'),
       ('C', '0', 'B'), ('C', '1', 'D'),
       ('D', '0', 'D'), ('D', '1', 'D')])

dm1 :: FSM [Int] 
dm1 =  ([[],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [0],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])



-- 1.
states :: FSM q -> [q]
alph   :: FSM q -> Alphabet
start  :: FSM q -> q
final  :: FSM q -> [q]
trans  :: FSM q -> [Transition q]


states (x,_,_,_,_) = x
alph   (_,x,_,_,_) = x
start  (_,_,x,_,_) = x
final  (_,_,_,x,_) = x
trans  (_,_,_,_,x) = x


-- 2.
delta :: (Eq q) => FSM q -> q -> Char -> [q]
delta m q a = [ x | (y,z,x) <- (trans m) , y == q && z == a ]


-- 3.
acceptFrom :: (Eq q) => FSM q -> q -> String -> Bool
acceptFrom m st []       = elem q (final m)
acceptFrom m st (x:xs)   = or [ acceptFrom m y x | y <- (delta m st x) ]

accepts :: (Eq q) => FSM q -> String -> Bool
accepts m xs = acceptFrom m (start m) xs

-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical xs = nub (sort xs)

-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
ddelta m xs a = canonical [ y | (b,c,y) <- (trans m), x <- xs, b == x && c == a ]


-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next m xs = [ ddelta m x y | x <- xs, y <- (alph m) ]


-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable m xs | next m xs /= xs = reachable m (next m xs)
               | otherwise       = xs

-- 8.
h :: (Ord q) => FSM q -> [q] -> Bool
h m xs = or [ y == x | x <- xs, y <- (final m) ]

dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal m xs = [ x | x <- xs, h m x ]


-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans m xs = [ (x,a, delta m x a) | x <- xs, a<- alph m ]


-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q]
deterministic = undefined


