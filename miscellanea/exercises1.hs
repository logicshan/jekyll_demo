import Test.QuickCheck
import Data.Char (isUpper)
import Data.List (find, intersperse)
import Data.Maybe (isJust)

-- =====================================================
-- 1. Warmup
-- Create 3 implementations of the function, which takes list of Int-s
-- and returns a list of signs (1 or -1) of the elements in
-- original list. Zeroes shuold be skipped. 
-- Example: signsRec [3, 0, -2, -1] should return [1, -1, -1]

-- recursive definition
signsRec :: [Int] -> [Int]
signsRec []     = []
signsRec (0:xs) = signsRec xs
signsRec (x:xs) = signum x : signsRec xs

-- non-recursive definition using list comprehension
signsListComp :: [Int] -> [Int]
signsListComp xs = [ signum x | x <- xs, x /= 0 ]

-- non-recursive definition using appropriate higher order 
-- functions from Prelude
signsComb :: [Int] -> [Int]
signsComb = map signum . filter (/=0)

-- testing property
-- use this with QuickCheck to test your implementations
-- against each-other
prop_signs :: [Int] -> Bool
prop_signs xs = signsRec xs == signsListComp xs
                && signsListComp xs == signsComb xs


-- ======================================================
-- 2. Longest string
-- Create a recursive and a non-recursive version of function
-- which returns the longest string from given list of strings.
-- If there are several strings with same length, prefer the leftmost.
longestString1Rec :: [String] -> String
longestString1Rec xs = aux xs ""
  where aux [] s = s
        aux (x:xs) s
          | length x > length s = aux xs x
          | otherwise = aux xs s

-- hint: fold
longestString1NonRec :: [String] -> String
longestString1NonRec = foldl (\acc x -> if length acc < length x then x else acc) ""

prop_longestString1 :: [String] -> Bool
prop_longestString1 xs = longestString1Rec xs == longestString1NonRec xs


-- ======================================================
-- 3. Longest string vol. 2
-- Same as previous exercise, but if there are several strings 
-- with same length, prefer the rightmost.
longestString2Rec :: [String] -> String
longestString2Rec xs = aux xs ""
  where aux [] s = s
        aux (x:xs) s = if length x < length s then aux xs s else aux xs x


longestString2NonRec :: [String] -> String
longestString2NonRec =  foldl (\acc x -> if length acc <= length x then x else acc) ""

prop_longestString2 :: [String] -> Bool
prop_longestString2 xs = longestString2Rec xs == longestString2NonRec xs


-- ======================================================
-- 4. Longest string vol. 2
-- Refactor common parts out of previous 2 exercises into a helper function
-- and reimplement one version of ex 2 and on version of 3 using new helper
-- function with suitable argument. Test them against earlier versions.
-- hint: that suitable argument may be a function
-- remark: If you already used such helper in previous exercises, 
-- then skip this exercise.

longestStringSmart :: (Int -> Int -> Bool) -> [String] -> String
longestStringSmart comp = foldl (\acc x -> if (\x y -> length x `comp` length y) acc x then x else acc) ""

longestString1Smart :: [String] -> String
longestString1Smart = longestStringSmart (<)

longestString2Smart :: [String] -> String
longestString2Smart = longestStringSmart (<=)

prop_longestString1Smart :: [String] -> Bool
prop_longestString1Smart xs = longestString1Smart xs == longestString2Smart xs


-- =================================================================
-- 5. All caps strings
-- Create 2 versions of the function, which takes a string list 
-- and returns list of all strings in given list which contain only 
-- capital letters. Choose between recursive definition, non-recursive
-- with list comprehension and non-recursive with higher-order functions
-- hint: Use Hoogle (http://www.haskell.org/hoogle/) to find relevant
-- predicates.
allCapsStringsVer1 :: [String] -> [String]
allCapsStringsVer1 [] = []
allCapsStringsVer1 (x:xs) = if all isUpper x then x : allCapsStringsVer1 xs
                            else allCapsStringsVer1 xs

allCapsStringsVer2 :: [String] -> [String]
allCapsStringsVer2 = filter (all isUpper)

prop_allCapsStrings :: [String] -> Bool
prop_allCapsStrings = \xs -> allCapsStringsVer1 xs == allCapsStringsVer2 xs


-- =================================================================
-- 6. First All caps string
-- This function should return Just leftmost string from given list, which 
-- is all-caps or Nothing, if there is no all-caps string in given list.
-- Again, implement 2 versions of your choice.

firstAllCapsStringVer1 :: [String] -> Maybe String
firstAllCapsStringVer1 [] = Nothing
firstAllCapsStringVer1 (x:xs) = if all isUpper x then Just x else firstAllCapsStringVer1 xs

firstAllCapsStringVer2 :: [String] -> Maybe String
firstAllCapsStringVer2 = find (all isUpper)

prop_firstCapsStrings :: [String] -> Bool
prop_firstCapsStrings = \xs -> firstAllCapsStringVer1 xs == firstAllCapsStringVer2 xs


-- =================================================================
-- 7. All somethings
-- The function should answer, whether given function returns something
-- (ie. a Just) for each element in given list
-- Choose your favourite implementation strategy. If it's recursive definition,
-- then choose one more :)
yieldsSomethingForAll :: (a -> Maybe b) -> [a] -> Bool
yieldsSomethingForAll p = all (isJust . p)

-- ===========================================================================
-- 8. Put between
-- Part 1. Implement function which takes a separator and a list and returns
-- a new list with separator between neighbours in original list.
-- Eg. putBetween 0 [1,2,3,4] should return [1,0,2,0,3,0,4]
-- Part 2. Use Hoogle (http://www.haskell.org/hoogle/) to find existing std 
-- library function, which does the same thing and test your implementation 
-- against it. 
-- Hint: You can search by name or by required type in Hoogle
-- Part 3. Next to the documentation of the function in question, find the 
-- "source" link and compare your implementation with library implementation.
putBetween :: a -> [a] -> [a]
putBetween _ [] = []
putBetween _ [x] = [x]
putBetween i (x:xs) = x : i : putBetween i xs

prop_putBetween :: Int -> [Int] -> Bool
prop_putBetween y xs = putBetween y xs == intersperse y xs

-- ======================================================================
-- Here is a datatype representing a playing card:
data Suit = Clubs 
          | Diamonds
          | Hearts
          | Spades
          deriving (Show, Enum)
          
data Rank = Jack | Queen | King | Ace 
          | Number Int
          deriving Show

data Card = Card Suit Rank 
            deriving Show


-- ---------------------------------------------------------------------
-- 9. Create full deck of 52 cards (ie. number cards start from 2)
fullDeck :: [Card]
fullDeck = [ Card s r | s <- [Clubs .. Spades],
                        r <- map Number [2..10] ++ [Jack, Queen, King, Ace]]


-- 10. Create a function which decides whether given cards are all same color
sameColor :: [Card] -> Bool
sameColor = undefined


-- Optional exercise: make Card instance of Ord