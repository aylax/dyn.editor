module Geometry.Record
(
) where

data Person = Person {
firstName :: String
, lastName :: String
, age :: Int

} deriving(Show)


-- Manod
class Box m where
    ret :: a -> m a
    (<|>) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    x >> y = x <|> \_ -> y
    fail :: String -> m a
    fail msg = error msg


-- Option Eq Maybe
data Option v = None | Some v deriving(Show)

instance Box Option where
    ret x = Some x
    None <|> f = None
    Some x <|> f = f x
    fail _ = None


instance (Eq m) => Eq (Option m) where
    Some a == Some b = a == b
    None == None = True
    _ == _ = False


instance Functor Option where
    fmap f (Some a) = Some (f a)
    fmap f None = None
       