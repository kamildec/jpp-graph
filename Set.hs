module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems
              ) where
import Prelude hiding(null)
import Data.List(sort)

data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)

empty :: Set a
empty = Empty

null :: Set a -> Bool
null Empty = True
null _ = False

member :: Eq a => a -> Set a -> Bool
member _ Empty = False
member el (Singleton s) = el == s
member el (Union a b) = member el a || member el b

singleton :: a -> Set a
singleton = Singleton

fromList :: [a] -> Set a
fromList [] = Empty
fromList [x] = Singleton x
fromList (x:xs) = Union (Singleton x) (fromList xs)

toList :: Set a -> [a]
toList Empty = []
toList (Singleton s) = [s]
toList (Union s1 s2) = toList s1 ++ toList s2

unique :: Eq a => [a] -> [a]
unique [] = []
unique [x] = [x]
unique (x:xs) =
  if x == head xs then
    unique xs
  else
    x:unique xs

toAscList :: Ord a => Set a -> [a]
toAscList s = unique (sort (toList s))

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union s1 Empty = s1
union Empty s2 = s2
union s1 s2 = Union s1 s2

insert :: a -> Set a -> Set a
insert el Empty = Singleton el
insert el s = Singleton el `union` s

instance Ord a => Eq (Set a) where
  s1 == s2 = toAscList s1 == toAscList s2

instance Semigroup (Set a) where
  (<>) = union

instance Monoid (Set a) where
  mempty = Empty

instance Show a => Show (Set a) where
  show Empty = ""
  show (Singleton s) = show s
  show (Union s1 s2) = show s1 ++ ", " ++ show s2

instance Functor Set where
  fmap _ Empty = Empty
  fmap f (Singleton s) = Singleton (f s)
  fmap f (Union s1 s2) = Union (fmap f s1) (fmap f s2)
