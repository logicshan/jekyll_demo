{-# LANGUAGE OverlappingInstances, FlexibleInstances, TypeSynonymInstances #-}

import Control.Arrow

data BST k v = Empty
             | Bind k v (BST k v) (BST k v)
             deriving (Show)

insert k v (Bind k' v' l r)
  | k == k' = Bind k v l r
  | k < k' = Bind k' v' (insert k v l) r
  | otherwise = Bind k' v' l (insert k v r)
insert k v Empty = Bind k v Empty Empty

find k (Bind k' v' l r)
  | k == k' = Just v'
  | k < k' = find k l
  | otherwise = find k r
find k Empty = Nothing

ofList = foldl (\t (k, v) -> insert k v t) Empty

t = ofList [("chimichanga", 5.25),
            ("burrito"    , 4.50),
            ("frijoles"   , 2.75)]

foldBST f b Empty = b
foldBST f b (Bind k v l r) = f k v (foldBST f b l) (foldBST f b r)

toList = foldBST (\k v l r -> l ++ [(k, v)] ++ r) []

instance (Eq k, Eq v) => Eq (BST k v) where
  t1 == t2 = toList t1 == toList t2

{- each JSON object is either
     - a base value like a string, a number or a boolean
     - an (ordered) array of objects
     - a set of string-object pairs
-}
data JVal = JStr String
          | JNum Double
          | JBln Bool
          | JObj [(String, JVal)]
          | JArr [JVal]
          deriving (Eq, Ord, Show)

js1 =
  JObj [("name", JStr "Ranjit"),
        ("age", JNum 33),
        ("likes", JArr [JStr "guacamole", JStr "coffee", JStr "bacon"]),
        ("hates", JArr [JStr "waiting", JStr "grapefruit"]),
        ("lunches", JArr [JObj [("day",JStr "monday"),("loc",JStr "zanzibar")],
                          JObj [("day",JStr "tuesday"),("loc",JStr "farmers market")],
                          JObj [("day",JStr "wednesday"),("loc",JStr "hare krishna")],
                          JObj [("day",JStr "thursday"),("loc",JStr "faculty club")],
                          JObj [("day",JStr "friday"),("loc",JStr "coffee cart")]])]

doubleToJSON :: Double -> JVal
doubleToJSON = JNum

stringToJSON :: String -> JVal
stringToJSON = JStr

boolToJSON :: Bool -> JVal
boolToJSON = JBln

doublesToJSON :: [Double] -> JVal
doublesToJSON = JArr . map doubleToJSON

boolsToJSON :: [Bool] -> JVal
boolsToJSON = JArr . map boolToJSON

stringsToJSON :: [String] -> JVal
stringsToJSON = JArr . map stringToJSON

xsToJSON :: (a -> JVal) -> [a] -> JVal
xsToJSON f = JArr . map f

xysToJSON :: (a -> JVal) -> [(String, a)] -> JVal
xysToJSON f = JObj . map (second f)

lunches = [ [("day", "monday"),    ("loc", "zanzibar")] 
          , [("day", "tuesday"),   ("loc", "farmers market")]
          , [("day", "wednesday"), ("loc", "hare krishna")]
          , [("day", "thursday"),  ("loc", "faculty club")]
          , [("day", "friday"),    ("loc", "coffee cart")]
          ]

class JSON a where
  toJSON :: a -> JVal

instance JSON Double where
  toJSON = JNum

instance JSON Bool where
  toJSON = JBln

instance JSON String where
  toJSON = JStr

instance (JSON a) => JSON [a] where
  toJSON = JArr . map toJSON

instance (JSON a) => JSON [(String, a)] where
  toJSON = JObj . map (second toJSON)

instance (JSON a, JSON b) => JSON ((String, a), (String, b)) where
  toJSON ((k1, v1), (k2, v2)) = 
    JObj [(k1, toJSON v1), (k2, toJSON v2)]
 
instance (JSON a, JSON b, JSON c) => JSON ((String, a), (String, b), (String, c)) where
  toJSON ((k1, v1), (k2, v2), (k3, v3)) = 
    JObj [(k1, toJSON v1), (k2, toJSON v2), (k3, toJSON v3)]
 
instance (JSON a, JSON b, JSON c, JSON d) => JSON ((String, a), (String, b), (String, c), (String,d)) where
  toJSON ((k1, v1), (k2, v2), (k3, v3), (k4, v4)) = 
    JObj [(k1, toJSON v1), (k2, toJSON v2), (k3, toJSON v3), (k4, toJSON v4)]
 
instance (JSON a, JSON b, JSON c, JSON d, JSON e) => JSON ((String, a), (String, b), (String, c), (String,d), (String, e)) where
  toJSON ((k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5)) = 
    JObj [(k1, toJSON v1), (k2, toJSON v2), (k3, toJSON v3), (k4, toJSON v4), (k5, toJSON v5)]

hs = (("name", "Ranjit")
     ,("age", 33 :: Double)
     ,("likes", ["guacamole", "coffee", "bacon"])      
     ,("hates",["waiting", "grapefruit"])
     ,("lunches", lunches)
     )

instance (JSON v) => JSON (BST String v) where
  toJSON = JObj . map (second toJSON) . toList

hs' = (("name" , "el gordo taqueria"),
       ("address","213 Delicious Street"),
       ("menu", t))