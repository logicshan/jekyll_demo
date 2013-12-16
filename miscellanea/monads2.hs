newtype ST0 a = S0 {apply0 :: State -> (a, State)}

instance Monad ST0 where
  -- return :: a -> ST0 a
  return x = S0 (\s -> (x, s))
  
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S0 (\s -> let (x, s') = apply0 st s in apply0 (f x) s')

type State = Int

fresh :: ST0 Int
-- fresh = S0 (\n -> (n, n+1))
fresh = app (+1)

wtf1 = fresh >> fresh >> fresh >> fresh

wtf2 = fresh >>= \n1 ->
       fresh >>= \n2 ->
       fresh >>
       fresh >>
       return [n1, n2]

wtf3 = do n1 <- fresh
          fresh
          fresh
          fresh
          return n1

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving (Eq, Show)

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

mlabel :: Tree a -> ST0 (Tree (a, Int))
mlabel (Leaf x) = fresh >>= \n ->
                  return (Leaf (x, n))
mlabel (Node l r) = mlabel l >>= \l' ->
                    mlabel r >>= \r' ->
                    return (Node l' r')

label :: Tree a -> Tree (a, Int)
-- label t = fst (apply0 (mlabel t) 0)
label t = run (mlabel t) 0

app :: (State -> State) -> ST0 State
app f = S0 (\s -> (s, f s))

run :: ST0 a -> State -> a
run st = fst . apply0 st