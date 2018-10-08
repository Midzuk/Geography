module Lib
    ( grtCirDist
    ) where

import           Data.Semigroup (Semigroup(..))
import qualified Data.Vector    as V



type Lat = Double
type Lon = Double
data Coord = Coord Lat Lon

type Dist = Double

grtCirDist :: Coord -> Coord -> Dist
grtCirDist (Coord lat1 lon1) (Coord lat2 lon2) =
  6378137 * acos (sin lat1Rad * sin lat2Rad + cos lat1Rad * cos lat2Rad * cos (lon1Rad - lon2Rad))
  where
    lat1Rad = lat1 * pi / 180
    lon1Rad = lon1 * pi / 180
    lat2Rad = lat2 * pi / 180
    lon2Rad = lon2 * pi / 180



data Node = Node Int Coord deriving (Eq, Show)

data OD = Node :->: Node deriving (Eq, Show)
infixr 5 :->:

instance Ord OD where
  compare od1@(n1 :->: n2) od2@(n3 :->: n4)
    | od1 == od2 = EQ
    | otherwise =
      case compare n1 n3 of
        EQ ->
          if n2 < n4 then LT else GT
        o -> o

instance Semigroup OD where
  (<>) (n1 :->: n2) (n3 :->: n4)
    | n2 == n3 = n1 :->: n4
    | otherwise = error "Semigroup OD Error."



data Graph = Edge OD | Graph (V.Vector OD) deriving (Eq, Show)

{-
instance Eq Graph where
  g1 == g2 = compose g1 == compose g2
-}

instance Ord Graph where
  compare g1 g2
    | g1 == g2 = EQ
    | compose g1 < compose g2 = LT
    | otherwise = GT

instance Semigroup Graph where
  Edge od1 <> Edge od2 = Graph [od1, od2]
  Edge od <> Graph v = Graph $ V.cons od v
  Graph v <> Edge od = Graph $ V.snoc v od
  Graph v1 <> Graph v2 = Graph $ v1 <> v2



type Cost = Double

data Link = Link Coord Coord Graph Cost deriving (Eq, Show)

instance Ord Link where
  compare l1@(Link g1 c1) l2@(Link g2 c2)
    | l1 == l2 = EQ
    | c1 == c2 =
      if g1 < g2 then LT else GT
    | c1 < c2 = LT
    | otherwise = GT

instance Semigroup Link where
  Link g1 c1 <> Link g2 c2 = Link (g1 <> g2) (c1 + c2)

type Network = Map.Map OD Link

type Path = Network

compose :: Graph -> OD
compose (Edge od) = od
compose (Graph v) = foldr1 (<>) v

composeLink :: Link -> OD
composeLink (Link g _) = compose g
