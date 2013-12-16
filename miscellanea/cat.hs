import Prelude hiding (Functor)

class Category c where
  id :: c x x
  (.) :: c y z -> c x y -> c x z

type Hask = (->)

instance Category Hask where
  id x = x
  (g . f) x = g (f x)

class (Category c, Category d) => Functor c d t where
  fmap :: c a b -> d (t a) (t b)

newtype Id a = Id a

instance Functor Hask Hask Id where
  fmap f (Id a) = Id (f a)

type Endofunctor c t = Functor c c t