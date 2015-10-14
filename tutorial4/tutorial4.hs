-- Informatics 1 - Functional Programming 
-- Tutorial 4
--
-- Due: the tutorial of week 6 (23/24 Oct)

import Data.List (nub)
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/testpage.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br><b>TA:</b> "
            , "mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Don Sannella","dts@inf.ed.ac.uk")
               , ("Karoliina Lehtinen","m.k.lehtinen@sms.ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

-- 1.
sameString :: String -> String -> Bool
sameString xs ys = and [( toLower x) == (toLower y) | x <- xs, y <- ys] && length xs == length ys

sameStringR :: String -> String -> Bool
sameStringR [] []                                       = True
sameStringR (x:xs) (y:ys)  | length xs == length ys     = ((toLower x) == (toLower y)) && sameStringR xs ys
                           | otherwise                  = False

-- 2.


sameStringS :: String -> String -> Bool
sameStringS xs ys = and [( toLower x) == (toLower y) | x <- xs, y <- ys] && length xs <= length ys

prefix :: String -> String -> Bool
prefix [] ys                                                                = True
prefix xs []                                                                = False
prefix (x:xs) (y:ys)   | isAlpha x && ((toLower x) == (toLower y))          = True && prefix xs ys
                       | (isAlpha x == False) && (x == y)                   = True && prefix xs ys
                       | otherwise                                          = False

prefixL :: String -> String -> Bool
prefixL [] ys                   = False
prefixL xs []                    = True
prefixL (x:xs) (y:ys) | sameStringS xs ys       = True
                      | otherwise               = False

prop_prefix :: String -> Int -> Bool
prop_prefix str n  =  prefixL substr (map toLower str) &&
		      prefixL substr (map toUpper str)
                          where
                            substr  =  take n str


-- 3.
sameStringL :: String -> String -> Bool
sameStringL xs ys = and [( toLower x) == (toLower y) | (x,y) <- zip xs ys] && length xs >= length ys

contains :: String -> String -> Bool
contains [] ys                                                                  = False
contains xs []                                                                  = True
contains (x:xs) (y:ys) | sameStringL (x:xs) (y:ys) || contains (xs) (y:ys)      = True
                       | otherwise                                              = False
               
prop_contains :: String -> Int -> Int -> Bool
prop_contains = undefined


-- 4.
dif :: Char -> Char -> Bool
dif x y = if x == y then False else True


takeUntil :: String -> String -> String
takeUntil [] ys                                 = []
takeUntil xs []                                 = []
takeUntil xs (y:ys)   | not (prefix xs (y:ys)) && contains (y:ys) xs             = y : takeUntil xs ys
                      | prefix xs (y:ys)                                         = []
                      | not (contains (y:ys) xs)                                 = (y:ys)

-- difr 
-- difr = if dif x == False then [ x | x <- xs, ]

take2 :: String -> String -> String
take2 (x:xs) ys = takeWhile (dif x) ys

sameStringRev :: String -> String -> Bool
sameStringRev xs ys = and [( toLower x) == (toLower y) | (x,y) <- zip (reverse xs) (reverse ys)] && length xs <= length ys

suffix :: String -> String -> Bool
suffix [] ys                    = False
suffix xs []                    = True
suffix (x:xs) (y:ys) | sameStringRev xs ys     = True
                     | otherwise               = False

drop1 :: String -> String -> String
drop1 xs ys | not (contains ys xs)      = []
            | contains ys xs            = reverse (takeUntil (reverse xs) (reverse ys))

dropuntil :: String -> String -> String
dropuntil [] ys                                                         = []
dropuntil xs []                                                         = []
dropuntil xs (y:ys)  | contains (y:ys) xs && not (suffix xs (y:ys))     = drop (length (takeUntil xs (y:ys)) + length xs) (y:ys)
                     | contains (y:ys) xs && suffix xs (y:ys)           = []
                     | not (contains (y:ys) xs)                         = []
-- 5.
split :: String -> String -> [String]
split xs []                             = []
split [] ys                             = [ys]
split xs ys   | contains ys xs                          = takeUntil xs ys : split xs (dropuntil xs ys)
              | not (contains ys xs)                    = [ys]
              | contains xs (dropuntil xs ys) == False  = drop1 xs ys : []
              | suffix xs ys                            = []

reconstruct :: String -> [String] -> String
reconstruct xs [ys] = foldr (++) xs [ys]

--prop_split :: Char -> String -> String -> Bool
--prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
--  where sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML xs = split "<a href=" (dropuntil "<a href=" xs)

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails [xs] = drop 1 [xs]


-- 8.
-- link2pair :: Link -> (Name, Email)
-- link2pair xs = zip (dropuntil "">\" (takeUntil "</a>" xs)) (dropuntil "mailto:" (takeUntil ">" xs))


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML = undefined

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail = undefined


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML = undefined


-- Optional Material

-- 12.
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]
