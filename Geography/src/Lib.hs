{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}


module Network
    ( grtCirDist
    ) where

import           Data.Semigroup (Semigroup(..))
import qualified Data.Vector    as V
import qualified Data.Map.Lazy  as Map




type Lat = Double
type Lon = Double
data Coord = Coord { lat :: Lat, lon :: Lon } deriving (Eq, Show, Ord)

type Dist = Double

{-
grtCirDist :: Coord -> Coord -> Dist
grtCirDist (Coord lat1 lon1) (Coord lat2 lon2) =
  6378137 * acos (sin lat1Rad * sin lat2Rad + cos lat1Rad * cos lat2Rad * cos (lon1Rad - lon2Rad))
  where
    lat1Rad = lat1 * pi / 180
    lon1Rad = lon1 * pi / 180
    lat2Rad = lat2 * pi / 180
    lon2Rad = lon2 * pi / 180
-}

grtCirDist :: Coord -> Coord -> Dist
grtCirDist (Coord lat1 lon1) (Coord lat2 lon2) =
  6378137 * acos (sin (f lat1) * sin (f lat2) + cos (f lat1) * cos (f lat2) * cos (f lon1 - f lon2))
  where
    f = ((pi / 180) *)


type Node = Int

data OD = Node :->: Node deriving (Eq, Ord, Show)
infixr 5 :->:

instance Semigroup OD where
  (<>) (n1 :->: n2) (n3 :->: n4)
    | n2 == n3 = n1 :->: n4
    | otherwise = error "Semigroup OD error"

{-
directDist :: OD -> Dist
directDist ((coord -> c1) :->: (coord -> c2)) = grtCirDist c1 c2
-}


data Graph = Edge OD | Graph (V.Vector OD) deriving (Eq, Show)

compose :: Graph -> OD
compose (Edge od) = od
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



data Cost = Cost { costOrg :: Double, cost :: Double, costDest :: Double } deriving (Eq, Show)

instance Ord Cost where
  c1 <= c2 = costOrg c1 + cost c1 + costDest c1 <= costOrg c2 + cost c2 + costDest c2

data Link = Link OD Cost Graph deriving (Eq, Ord, Show)

instance Semigroup Link where
  Link od1 (Cost co1 c1 _) g1 <> Link od2 (Cost _ c2 cd2) g2
    | od1 == od2 = Link od1 (Cost co1 (c1 + c2) cd2) (g1 <> g2)
    | otherwise = error "Semigroup Link error"

composeLink :: Link -> OD
composeLink (Link _ _ g) = compose g
