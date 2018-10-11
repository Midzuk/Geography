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

--type Node = Int
type NodeId = Int
data Node = Node { nodeId :: NodeId, coordinates :: Coordinates } deriving (Eq, Ord, Show)

--data Link = (:->:) { origin :: Node, destination :: Node } deriving (Eq, Ord, Show)
--infixr 5 :->:

data Link = (:->:) { origin :: NodeId, destination :: Node, distance :: Distance } deriving (Eq, Ord, Show)
infixr 5 :->:

{-
instance Semigroup Link where
  (<>) (ni1 :->: (nodeId -> ni2)) (ni3 :->: n4)
    | ni2 == ni3 = ni1 :->: n4
    | otherwise = error "Semigroup Link error"
-}

{-
isNextLink :: Link -> Link -> Bool
(ni3 :->: (nodeId -> ni4)) `isNextLink` (ni1 :->: (nodeId -> ni2)) = ni2 == ni3 && ni1 /= ni4
-}

type Graph = V.Vector NodeId

{-
compose :: Graph -> Link
compose g = foldr1 (<>) g
-}

data Cost = Cost { costLink :: Distance, costDest :: Distance } deriving (Eq)

{-
instance Show Cost where
  show c = show (costLink c) <> "," <> show (costDest c)
-}

instance Ord Cost where
  c1 `compare` c2 =
    case (costLink c1 + costDest c1) `compare` (costLink c2 + costDest c2) of
      LT -> LT
      EQ -> costLink c1 `compare` costLink c2
      GT -> GT

instance Semigroup Cost where
  c1 <> c2 =
    Cost
      { costLink = costLink c1 + costLink c2
      , costDest = costDest c2 }

data Path = Path { cost :: Cost, endNode :: NodeId, graph :: Graph } deriving (Eq)

instance Ord Path where
  p1 `compare` p2 =
    case cost p1 `compare` cost p2 of
      LT -> LT
      EQ -> (endNode p1, graph p1) `compare` (endNode p2, graph p2)
      GT -> GT

instance Semigroup Path where
  p1 <> p2 =
    Path
      { cost = cost p1 <> cost p2
      , endNode = endNode p2
      , graph = graph p1 <> graph p2 }

makePath :: Node -> Link -> Path
makePath dest l =
  Path
    { cost = Cost { costLink = distance l, costDest = grtCirDist (coordinates $ destination l) $ coordinates dest }
    , endNode = nodeId $ destination l
    , graph = [ origin l, nodeId $ destination l ] }

{-
composePath :: Path -> Link
composePath (Path _ g) = compose g
-}


--type Network = Set.Set Path

shortestPath :: Link -> V.Vector Link -> Path
shortestPath target ls_ = go ps_ rls_
  where
    (nls_, rls_) = V.partition (((==) `on` origin) target) ls_
    ps_ = V.foldr Set.insert Set.empty $ makePath (destination target) <$> nls_

    go :: Set.Set Path -> V.Vector Link -> Path
    go (Set.deleteFindMin -> (p, rps)) ls
      | endNode p == nodeId (destination target) = p
      | otherwise =
        let
          (nls, rls) = V.partition ((endNode p ==) . origin) ls
          ps = V.foldr Set.insert Set.empty $ (p <>) . makePath (destination target) <$> nls
        in
          go (rps <> ps) rls
{-
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
-}