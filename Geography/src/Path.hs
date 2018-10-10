{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}


module Path where

import           Data.Semigroup (Semigroup(..))
import qualified Data.Vector    as V
import qualified Data.Map.Lazy  as Map
import qualified Data.Set       as Set



type Lat = Double
type Lon = Double
data Coord = Coord { lat :: Lat, lon :: Lon } deriving (Eq, Show, Ord)
data OD = OD Coord Coord deriving (Eq, Show, Ord)

type Dist = Double



grtCirDist :: Coord -> Coord -> Dist
grtCirDist (Coord lat1 lon1) (Coord lat2 lon2) =
  6378137 * acos (sin (f lat1) * sin (f lat2) + cos (f lat1) * cos (f lat2) * cos (f lon1 - f lon2))
  where
    f = ((pi / 180) *)


type Node = Int

data Link = Node :->: Node deriving (Eq, Ord, Show)
infixr 5 :->:

instance Semigroup Link where
  (<>) (n1 :->: n2) (n3 :->: n4)
    | n2 == n3 = n1 :->: n4
    | otherwise = error "Semigroup Link error"

isNextLink :: Link -> Link -> Bool
(n3 :->: n4) `isNextLink` (n1 :->: n2) = n2 == n3 && n1 /= n4



data Graph = Edge Link | Graph (V.Vector Link) deriving (Eq, Show)

compose :: Graph -> Link
compose (Edge l) = l
compose (Graph v) = foldr1 (<>) v

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



data Cost = Cost { costOrg :: Double, costLink :: Double, costDest :: Double } deriving (Eq)

instance Show Cost where
  show c = show (costOrg c) <> "," <> show (costLink c) <> "," <> show (costDest c)

instance Ord Cost where
  c1 <= c2 = costOrg c1 + costLink c1 + costDest c1 <= costOrg c2 + costLink c2 + costDest c2

data Path = Path { cost :: Cost, graph :: Graph } deriving (Eq, Ord, Show)

instance Semigroup Path where
  Path (Cost co1 c1 _) g1 <> Path (Cost _ c2 cd2) g2 = Path (Cost co1 (c1 + c2) cd2) (g1 <> g2)

composePath :: Path -> Link
composePath (Path _ g) = compose g



type Network = Set.Set Path

shortestPath :: Link -> Network -> Path
shortestPath = go Map.empty
  where
    go :: Map.Map Link Path -> Link -> Network -> Path
    go pm l0 (Set.deleteFindMin -> (p, nw))
      | l == l0 = p
      | otherwise = go (Map.insert l p pm) l0 nw1
      where
        l = composePath p

        ps1 = (p <>) . snd <$> filter (\(l2, _) -> Map.notMember (l <> l2) pm) (Map.assocs $ Map.filterWithKey (\l2 _ -> l2 `isNextLink` l) pm)
        ps2 = (<> p) . snd <$> filter (\(l1, _) -> Map.notMember (l1 <> l) pm) (Map.assocs $ Map.filterWithKey (\l1 _ -> l `isNextLink` l1) pm)

        nw1 = foldr Set.insert nw (ps1 <> ps2)