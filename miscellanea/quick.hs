import Test.QuickCheck
import Data.List hiding (insert)

-- quickCheck :: Testable prop => prop -> IO ()

prop_RevRev :: [Int] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs

prop_RevApp :: [Int] -> [Int] -> Bool
prop_RevApp xs ys = reverse (xs ++ ys)
                    == reverse ys ++ reverse xs

prop_RevWrong :: [Int] -> [Int] -> Bool
prop_RevWrong xs ys = reverse (xs ++ ys)
                      == reverse xs ++ reverse ys

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort = foldr insert []

ordered :: Ord a => [a] -> Bool
ordered (x:y:ys) = x <= y && ordered (y:ys)
ordered ys = True

prop_sortOrder :: [Int] -> Bool
prop_sortOrder xs = ordered (isort xs)

sameElems :: Eq a => [a] -> [a] -> Bool
sameElems xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_sortElems :: [Int] -> Bool
prop_sortElems xs = sameElems xs (isort xs)

prop_insertOrder1 :: Int -> [Int] -> Bool
prop_insertOrder1 x xs = ordered xs `implies`
                         ordered (insert x xs)

implies :: Bool -> Bool -> Bool
implies x y = not x || y

prop_insertOrder2 :: Int -> [Int] -> Property
prop_insertOrder2 x xs = ordered xs ==>
                         ordered (insert x xs)

prop_insertOrder3 :: Int -> Property
prop_insertOrder3 x = forAll orderedList (\xs -> ordered (insert x xs))