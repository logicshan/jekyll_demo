data Doc = Nil
         | Text String
         | Doc :<> Doc
         | Line
         | Nest Int Doc
         | Doc :<|> Doc

group :: Doc -> Doc
group x = flatten x :<|> x

flatten :: Doc -> Doc
flatten Nil          = Nil
flatten (Text s)     = Text s
flatten (d1 :<> d2)  = flatten d1 :<> flatten d2
flatten Line         = Text " "
flatten (Nest i d)   = Nest i (flatten d)
flatten (d1 :<|> d2) = flatten d1

data Tree = Node String [Tree]

tree = Node "aa" [ Node "b" [ Node "c" [] ],
                   Node "dd" [],
                   Node "e" [ Node "f" [] ] ]

pretty :: Int -> Doc -> String
pretty w d = convert w 0 [(d,0)]

convert :: Int -> Int -> [(Doc, Int)] -> [Char]
convert w c []             = ""
convert w c ((Nil,i) : ds) = convert w c ds
convert w c ((Text s,i) : ds)
            = s ++ convert w (c + length s) ds
convert w c ((d1 :<> d2,i) : ds)
            = convert w c ((d1,i):(d2,i):ds)
convert w c ((Line,i) : ds)
            = "\n" ++ copy i ' ' ++ convert w i ds
convert w c ((Nest n d,i) : ds)
            = convert w c ((d,n+i):ds)
convert w c ((d1 :<|> d2,i) : ds)
           = better w c (convert w c ((d1,i):ds))
                        (convert w c ((d2,i):ds))

better w c s1 s2 = if fits (w-c) s1 then s1 else s2
fits w xs = w >= 0 && (null xs || head xs == '\n'
                               || fits (w-1) (tail xs))

copy 0 x = []
copy n x = x : copy (n-1) x

nil = Nil
text = Text
(<>) = (:<>)
line = Line
nest = Nest

ppTree (Node s ts) = group (text s <> nest (length s) (ppBracket ts))
  where ppBracket [] = nil
        ppBracket ts = text "[" <>
                       nest 1 (ppTrees ts) <>
                       text "]"
        ppTrees [t] = ppTree t
        ppTrees (t:ts) = ppTree t <> text "," <>
                         line <> ppTrees ts