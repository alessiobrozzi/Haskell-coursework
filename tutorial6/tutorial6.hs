-- Informatics 1 Functional Programming
-- Tutorial 6
--
-- Due: 6/7 November

import System.Random


-- Importing the keymap module

import KeymapList


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

bar :: (Barcode, Item)
bar = ("282344",("ahhsh","ahdjd"))

bars :: [(Barcode, Item)]
bars = [("12443",("abcb","abcjd")),
        ("48494849",("jfjfj","hfjfj"))]

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen bars = maximum [ length a | (_,(a,_)) <- bars]

formatLine :: Int -> (Barcode, Item) -> String
formatLine n (x,(a,b)) = x ++ "..." ++ a ++ "..." ++ (replicate (n - length a) '.') ++ b

showCatalogue :: Catalogue -> String
showCatalogue = undefined
     
-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe [a] = Just (head [a])

catMaybes :: [Maybe a] -> [a]
catMaybes xs = concat [ maybeToList x | x <- xs]

-- Exercise 3

-- barc :: [Barcode]
-- barc = ["262626","727272","828282"]

getItems :: [Barcode] -> Catalogue -> [Item]
getItems barc testDB = catMaybes [ get x testDB | x <- barc]






-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
