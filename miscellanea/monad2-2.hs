module MyState (ST, get, put, apply) where

import Data.Map

newtype ST s a = S {apply :: s -> (a, s)}

instance Monad (ST s) where
  return x = S (\s -> (x, s))
  st >>= f = S (\s -> let (x, s') = apply st s
                     in apply (f x) s')

get = S (\s -> (s, s))

put s' = S (const ((), s'))

freshS :: ST Int Int
freshS = get >>= \n ->
         put (n+1) >>
         return n

mlabelS :: Tree a -> ST Int (Tree (a, Int))
mlabelS (Leaf x) = do n <- freshS
                      return (Leaf (x, n))
mlabelS (Node l r) = do l' <- mlabelS l
                        r' <- mlabelS r
                        return (Node l' r')

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving (Eq, Show)

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

data MySt a = M { index :: Int,
                  freq  :: Map a Int }
              deriving (Eq, Show)

freshM = do
  s <- get
  let n = index s
  put $ s { index = n + 1 }
  return n

freshM' = get >>= \s ->
          let n = index s in
          put (s { index = n + 1 }) >>
          return n


updFreqM k = do
  s <- get
  let f = freq s
  let n = findWithDefault 0 k f
  put $ s {freq = insert k (n + 1) f}

updFreqM' k = get >>= \s ->
              let f = freq s
                  n = findWithDefault 0 k f in
              put $ s {freq = insert k (n + 1) f}

mlabelM (Leaf x) = do updFreqM' x
                      n <- freshM'
                      return $ Leaf (x, n)

mlabelM (Node l r) = do l' <- mlabelM l
                        r' <- mlabelM r
                        return $ Node l' r'

initM = M 0 empty

tree2 = Node tree tree

(lt, s) = apply (mlabelM tree2) initM