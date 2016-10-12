infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

class FFunctor f where
  ffmap :: (a -> b) -> f a -> f b

instance FFunctor Maybe where
  ffmap f Nothing = Nothing
  ffmap f (Just x) = Just (f x)

instance FFunctor List where
  ffmap _ Empty = Empty
  ffmap f (x :-: xs) = f x :-: ffmap f xs
