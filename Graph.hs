module Graph where
import Set(Set)
import qualified Set as Set
import Data.List(sort)
class Graph g where
  empty   :: g a
  vertex  :: a -> g a
  union   :: g a -> g a -> g a
  connect :: g a -> g a -> g a

data Relation a = Relation { domain :: Set a, relation :: Set (a, a) }
    deriving (Eq, Show)

data Basic a = Empty
             | Vertex a
             | Union (Basic a) (Basic a)
             | Connect (Basic a) (Basic a)

instance Graph Relation where
  empty = Relation Set.empty Set.empty
  vertex v = Relation (Set.singleton v) Set.empty
  union g1 g2 = Relation (Set.union (domain g1) (domain g2)) (Set.union (relation g1) (relation g2))
  connect g1 g2 = Relation (Set.union (domain g1) (domain g2)) (
    Set.union
    (Set.union (relation g1) (relation g2))
    (Set.fromList [(x, y) | x<-Set.toList (domain g1), y<-Set.toList (domain g2)])
    )

instance (Ord a, Num a) => Num (Relation a) where
  fromInteger = vertex . fromInteger
  (+) = union
  (*) = connect
  signum = const empty
  abs = id
  negate = id

instance Graph Basic where
  empty = Empty
  vertex v = Vertex v
  union g1 g2 = Union g1 g2
  connect g1 g2 = Connect g1 g2

instance Ord a => Eq (Basic a) where
  g1 == g2 =
    let
      g11 = getRelation g1
      g22 = getRelation g2
    in
      g11 == g22

instance (Ord a, Num a) => Num (Basic a) where
    fromInteger = vertex . fromInteger
    (+)         = union
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Semigroup (Basic a) where
  (<>) = union

instance Monoid (Basic a) where
  mempty = Empty

fromBasic :: Graph g => Basic a -> g a
fromBasic Empty = empty
fromBasic (Vertex v) = vertex v
fromBasic (Union g1 g2) = fromBasic g1 `union` fromBasic g2
fromBasic (Connect g1 g2) = connect (fromBasic g1) (fromBasic g2)

-- Function converts Basic a to Relation a
getRelation :: Basic a -> Relation a
getRelation = fromBasic

-- Function checks if element x exists in list of pairs as first or second
isInPairList :: (Eq a) => a -> [(a, a)] -> Bool
isInPairList x [] = False
isInPairList x ((y1, y2):ys) = x == y1 || x == y2 || isInPairList x ys

-- Function returns list of elements which does not appear in list of pairs
notInPairList :: (Eq a) => [a] -> [(a, a)] -> [a]
notInPairList [] _ = []
notInPairList (x:xs) ys =
  if isInPairList x ys then notInPairList xs ys
  else x : notInPairList xs ys

-- Function eliminates duplicates in sorted list
unique :: Eq a => [a] -> [a]
unique [] = []
unique [x] = [x]
unique (x:xs) =
  if x == head xs then
    unique xs
  else
    x:unique xs

getDomainRelation :: Ord a => Basic a -> ([a], [(a, a)])
getDomainRelation g =
  let
    relationGraph = getRelation g
    domainGraphList = Set.toList (domain relationGraph)
    relationGraphList = unique(sort (Set.toList (relation relationGraph)))
    vertices = notInPairList (unique (sort domainGraphList)) relationGraphList
  in
    (vertices, relationGraphList)

instance (Ord a, Show a) => Show (Basic a) where
  show g =
    let
      (vertices,relation) = getDomainRelation g
    in
      (showString "edges " . showList relation . 
      showString " + vertices " . showList vertices) ""

-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

example34 :: Basic Int
example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17

relationToString :: Show a => [(a, a)] -> String
relationToString [] = ""
relationToString ((x, y):xs) =
  show x ++ " -> " ++ show y ++ ";\n" ++ relationToString xs

verticesToString :: Show a => [a] -> String
verticesToString [] = ""
verticesToString (x:xs) =
  show x ++ ";\n" ++ verticesToString xs

todot :: (Ord a, Show a) => Basic a -> String
todot g =
  let
    (vertices, relation) = getDomainRelation g
  in
    (showString "digraph {\n" . showString (relationToString relation) . 
    showString (verticesToString vertices) . showString "}") ""

instance Functor Basic where
  fmap _ Empty = Empty
  fmap f (Vertex v) = Vertex (f v)
  fmap f (Union g1 g2) = Union (fmap f g1) (fmap f g2)
  fmap f (Connect g1 g2) = Connect (fmap f g1) (fmap f g2)

-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]

mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV a b c = fmap (\x -> if x == a || x == b then c else x)

instance Applicative Basic where
  pure x = Vertex x
  (<*>) Empty _ = Empty
  (<*>) (Vertex f) g = fmap f g
  (<*>) (Union f1 f2) g = Union (f1 <*> g) (f2 <*> g)
  (<*>) (Connect f1 f2) g = Connect (f1 <*> g) (f2 <*> g)

instance Monad Basic where
  (>>=) Empty _ = Empty
  (>>=) (Vertex v) f = f v
  (>>=) (Union g1 g2) f = Union (g1 >>= f) (g2 >>= f)
  (>>=) (Connect g1 g2) f = Connect (g1 >>= f) (g2 >>= f)

-- | Split Vertex
-- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
splitV a b c g = 
  let
    g1 = (g >>= \x -> if x == a then Vertex b else Vertex x)
    g2 = (g >>= \x -> if x == a then Vertex c else Vertex x)
  in
    Union g1 g2
