data Queue a = Queue [a] [a]  deriving (Show)

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push a (Queue b c) = Queue b (a:c)

pop :: Queue a -> Queue a
pop (Queue [] []) = Queue [] []
pop (Queue [] c) =  Queue (reverse (init c)) []
pop (Queue (a:b) c) = Queue b c

top :: Queue a -> a
top (Queue [] c) = last c
top (Queue (a:b) c) = a

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty (Queue _ _) = False


instance Eq a => Eq (Queue a) where
  Queue [] [] == Queue [] [] = True
  Queue a b == Queue c d = a++(reverse b) == c++(reverse d)
