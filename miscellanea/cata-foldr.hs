newtype Fix f = Fx (f (Fix f))
type Algebra f a = f a -> a

unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
-- show
data ListF a b = Nil | Cons a b

instance Functor (ListF a) where
    fmap f Nil = Nil
    fmap f (Cons e x) = Cons e (f x)

algSum :: Algebra (ListF Int) Int
algSum Nil = 0
algSum (Cons e acc) = e + acc

lst :: Fix (ListF Int)
lst = Fx $ Cons 2 (Fx $ Cons 3 (Fx $ Cons 4 (Fx Nil)))

main = do
    print $ cata algSum lst
    print $ foldr (\e acc -> e + acc) 0 [2, 3, 4]