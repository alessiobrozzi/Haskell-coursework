-- Informatics 1 - Functional Programming 
-- Tutorial 7
--
-- Week 9 - Due: 13/14 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split (Go x) 	= [Go x]
split (Turn x) 	= [Turn x]
split (Sit)     = []
split (p :#: q) = (split p) ++ (split q)

-- 1b. join
join :: [Command] -> Command
join []              = Sit
join (p:q)           = (p :#: (join q))
join [x]             = x

join1 :: [Command] -> Command
join1 = foldr (:#:) Sit

-- 1c  equivalent
equivalent :: Command -> Command -> Bool
equivalent x y = split x == split y

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join x = join (split x) == x

prop_split :: Command -> Bool
prop_split xs = and [ x == Sit | x <- split xs]


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy 0 xs         = Sit
copy n xs | n > 0 = xs :#: copy (n-1) xs

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon x = copy 5 (Go x :#: Turn 72)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon x y = copy y (Go x :#: Turn (360/fromIntegral y)
                      -- (180 - ((((fromIntegral y)-2)*180)/(fromIntegral y))))



-- Exercise 3
-- spiral
--spiral :: Distance -> Int -> Distance -> Angle -> Command
--spiral side 0 step angle = Sit
--spiral side n step angle | side > 0  = (Go side :#: Turn angle) :#: spiral (side + step) (n-1) step angle
               

-- Exercise 4
-- (optimise
-- optimise :: Command -> Command
-- optimise com = join (f (split com))
--                     where f [] = []
--                          f (Sit : com) = f com
--                          f (Go d : Go d' : com) = f(Go (d + d') : com)
--                          f (Go 0 : com) = f com
--                          f Turn g : Turn g' : com) = f(Turn (g + g') : com)
--                      f (Turn 0 : com) = f com
--                          f (c : com) = c : f com)


opt :: Command -> Command
opt (Sit)          = Sit
opt (Go a)         = Go a
opt (Turn a)       = Turn a
opt (Go a :#: Go b) = Go (a+b)
opt (Turn a :#: Turn b) = Turn (a+b)
opt (p :#: Sit)     = opt p
opt (Sit :#: p)     = opt p
opt (Go 0 :#: p)    = opt p
opt (p :#: Go 0)    = opt p
opt (Turn 0 :#: p)  = opt p
opt (p :#: Turn 0)  = opt p
opt (p :#: q)       = opt (opt p :#: opt q)

o [] = []
o (Go a : Go b : xs) = f (Go (a + b) :xs)
o (Turn a : Turn b : xs) = f (Turn (a+b) : xs)
o (x : xs) = x : f xs

op = join.o.filter(/=Go 0).o.filter(/= Turn 0).o.split

-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = p :#: f x
   where
   f 0      = Go 10
   f x      = g (x-1) :#: n :#: f (x-1) :#: n :#: g (x-1)
   g 0      = Go 10
   g x      = f (x-1) :#: p :#: g (x-1) :#: p :#: f (x-1)
   n        = Turn 60
   p        = Turn (-60)   

-- 6. snowflake
snowflake :: Int -> Command
snowflake x = p :#: f x
   where
   f 0      = f (x-1) :#: p :#: p :#: f(x-1) :#: p :#: p :#: f(x-1) :#: p :#: p
   f x      = f (x-1) :#: n :#: f (x-1) :#: p :#: p f (x-1) :#: n :#: f (x-1)
   n        = Turn 60
   p        = Turn (-60)

-- 7. hilbert
hilbert :: Int -> Command
hilbert x = p :#: l x
   where 
   l 0    = l (x-1)
   l x    = n :#: r (x-1) :#: f (x-1) :#: p :#: l (x-1) :#: f (x-1) :#: l (x-1) :#: p :#: f (x-1) :#: r (x-1) :#: p
   r 0    = Go 10
   r x    = p :#: l (x-1) :#: f (x-1) :#: n :#: r (x-1) :#: f (x-1) :#: r (x-1) :#: n :#: f (x-1) :#: l (x-1) :#: p

