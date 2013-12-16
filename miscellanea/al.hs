data Algebra f c = Algebra (f c -> c)
data F1 x = Zero | One | Plus x x
data ListF a x = Nil | Cons a x

f :: F1 Int -> Int
f Zero       = 0
f One        = 1
f (Plus x y) = x + y

g :: F1 [Int] -> [Int]
g Zero = []
g One  = [1]
g (Plus x y) = x ++ y

alg1 :: Algebra F1 Int
alg1 = Algebra f

alg2 :: Algebra F1 [Int]
alg2 = Algebra g

h :: ListF b Int -> Int
h Nil = 0
h (Cons x xs) = 1 + xs

alg3 :: Algebra (ListF a) Int
alg3 = Algebra h

data Initial alg = Init (alg (Initial alg))

ex1 :: Initial F1
ex1 = Init(Plus (Init One) (Init Zero))

ex2 :: Initial (ListF Int)
ex2 = Init(Cons 2 (Init Nil))

initialAlg :: Algebra f (Initial f)
initialAlg = Algebra Init

len :: Num a => Initial (ListF b) -> a
len (Init Nil) = 0
len (Init (Cons x xs)) = 1 + len xs

app :: Initial (ListF a) ->
      Initial (ListF a) -> Initial (ListF a)
app (Init Nil) ys = ys
app (Init (Cons x xs)) ys = Init(Cons x (app xs ys))

data Arrow f a b =
  Arr (Algebra f a) (Algebra f b) (a -> b)

valid :: (Eq b, Functor f) =>
         Arrow f a b -> f a -> Bool
valid (Arr (Algebra f) (Algebra g) h) x =
  h(f x) == g(fmap h x)