{-# LANGUAGE DeriveFunctor #-}
data ExprF r = Const Int
             | Add r r
             | Mul r r
             deriving Functor

newtype Fix f = Fx (f (Fix f))

type Expr = Fix ExprF

unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

alg :: ExprF Int -> Int
alg (Const i)   = i
alg (x `Add` y) = x + y
alg (x `Mul` y) = x * y

eval :: Expr -> Int
eval = cata alg
-- show
testExpr :: Expr
testExpr = Fx $
           (Fx $ (Fx $ Const 2) `Add` (Fx $ Const 3)) `Mul` 
           (Fx $ Const 4)

main :: IO ()
main = print $ eval testExpr