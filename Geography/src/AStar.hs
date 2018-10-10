{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}


module AStar where

import           Data.Semigroup (Semigroup(..))
import qualified Data.Vector    as V
import qualified Data.Map.Lazy  as Map
import qualified Data.Set       as Set
import Data.Function (on)


type Latitude = Double
type Longitude = Double
data Coordinates = Coordinates { latitude :: Latitude, longitude :: Longitude } deriving (Eq, Show, Ord)
data OD = OD Coordinates Coordinates deriving (Eq, Show, Ord)

type Distance = Double

grtCirDist :: Coordinates -> Coordinates -> Distance
grtCirDist (Coordinates lat1 lon1) (Coordinates lat2 lon2) =
  6378137 * acos (sin (f lat1) * sin (f lat2) + cos (f lat1) * cos (f lat2) * cos (f lon1 - f lon2))
  where
    f = ((pi / 180) *)

type Node = Int

data Link = (:->:) { origin :: Node, destination :: Node } deriving (Eq, Ord, Show)
infixr 5 :->:

instance Semigroup Link where
  (<>) (n1 :->: n2) (n3 :->: n4)
    | n2 == n3 = n1 :->: n4
    | otherwise = error "Semigroup Link error"

isNextLink :: Link -> Link -> Bool
(n3 :->: n4) `isNextLink` (n1 :->: n2) = n2 == n3 && n1 /= n4

type Graph = V.Vector Link

compose :: Graph -> Link
compose g = foldr1 (<>) g

data Cost = Cost { costLink :: Double, costDest :: Double } deriving (Eq)

instance Show Cost where
  show c = show (costLink c) <> "," <> show (costDest c)

instance Ord Cost where
  c1 <= c2 = costLink c1 + costDest c1 <= costLink c2 + costDest c2

data Path = Path { cost :: Cost, graph :: Graph } deriving (Eq, Ord, Show)

instance Semigroup Path where
  Path (Cost cl1 _) g1 <> Path (Cost cl2 cd2) g2 = Path (Cost (cl1 + cl2) cd2) (g1 <> g2)

composePath :: Path -> Link
composePath (Path _ g) = compose g



type Network = Set.Set Path

shortestPath :: Link -> Set.Set Path -> Path
shortestPath l ps = go psi pso
  where
    (psi, pso) = Set.partition (\_p -> ((==) `on` origin) (compose $ graph _p) l) ps

    go (Set.deleteFindMin -> (p1, psi1)) pso1 =
      let
        (psi2, pso2) = Set.partition (\_p -> (isNextLink `on` (compose . graph)) _p p1) pso1
      in
        if compose (graph p1) == l
          then p1
          else go (psi1 <> (Set.map (p1 <>) psi2)) pso2 
