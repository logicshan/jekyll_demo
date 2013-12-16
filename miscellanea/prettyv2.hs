data Doc = Nil
         | Text String
         | Doc :<> Doc
         | Line
         | Nest Int Doc

nil = Nil
text = Text
(<>) = (:<>)
line :: Doc
line = Line
nest :: Int -> Doc -> Doc
nest = Nest

data Tree = Node String [Tree]

tree = Node "aa" [ Node "b" [ Node "c" [] ],
                   Node "dd" [],
                   Node "e" [ Node "f" [] ] ]

pretty d = convert [(d,0)]

convert [] = ""
convert ((Nil,i) : ds) = convert ds
convert ((Text s,i) : ds) = s ++ convert ds
convert ((d1 :<> d2,i) : ds) = convert ((d1,i):(d2,i):ds)
convert ((Line,i) : ds) = "\n" ++ copy i ' ' ++ convert ds
convert ((Nest n d,i) : ds) = convert ((d,n+i):ds)

ppTree (Node s ts) = text s <>
                     nest (length s) (ppBracket ts)
  where ppBracket [] = nil
        ppBracket ts = text "[" <>
                       nest 1 (ppTrees ts) <>
                       text "]"
        ppTrees [t] = ppTree t
        ppTrees (t:ts) = ppTree t <> text "," <>
                         line <> ppTrees ts

copy 0 x = []
copy n x = x : copy (n-1) x