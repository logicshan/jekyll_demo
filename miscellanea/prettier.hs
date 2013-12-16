import Control.Monad

data Doc = Nil
         | String `Text` Doc
         | Int `Line` Doc

nil = Nil
text s = s `Text` Nil
line = 0 `Line` Nil

(s `Text` x) <> y = s `Text` (x <> y)
(i `Line` x) <> y = i `Line` (x <> y)
Nil <> y = y

nest i (s `Text` x) = s `Text` nest i x
nest i (j `Line` x) = (i+j) `Line` nest i x
nest i Nil = Nil

layout (s `Text` x) = s ++ layout x
layout (i `Line` x) = '\n' : copy i ' ' ++ layout x
layout Nil = ""

copy :: (Eq a, Num a) => a -> a1 -> [a1]
copy 0 x = []
copy n x = x : copy (n-1) x

txt :: Doc
txt = text "bbbbb[" <>
      nest 2 line <> text "ccc," <>
      nest 2 line <> text "dd" <>
      nest 0 line <> text "]"

data Tree = Node String [Tree]

showTree (Node s ts) = text s <> nest (length s) (showBracket ts)

showBracket [] = nil
showBracket ts = text "[" <> nest 1 (showTrees ts) <> text "]"

showTrees [t] = showTree t
showTrees (t:ts) = showTree t <> text "," <> line <> showTrees ts

t = Node "aaa" [(Node "bbbbb" [(Node "ccc" []), (Node "dd" [])]),
                (Node "eee" []),
                (Node "ffff" [(Node "gg" []), (Node "hhh" []), (Node "ii" [])])]

main :: IO ()
main = liftM layout (return (showTree t)) >>= putStrLn