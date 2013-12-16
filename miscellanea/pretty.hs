data Expr = Num Int | Add Expr Expr

showExprv0 (Num x)     = show x
showExprv0 (Add e1 e2) = showExprv0 e1 ++
                         "+"         ++
                         showExprv0 e2

nil    :: Doc
text   :: String -> Doc
(<>)   :: Doc -> Doc -> Doc
pretty :: Doc -> String

showExprv1 = pretty . showE
  where showE (Num x) = text (show x)
        showE (Add e1 e2) = showE e1 <>
                            text "+" <>
                            showE e2

data Doc = Nil
         | Text String
         | Doc :<> Doc

nil = Nil
text = Text
(<>) = (:<>)

pretty d = convert [d]

convert []               = ""
convert (Nil : ds)       = convert ds
convert (Text s : ds)    = s ++ convert ds
convert (d1 :<> d2 : ds) = convert (d1:d2:ds)