{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}


module CSV where

import qualified Data.ByteString.Lazy as B
import           Data.Csv             (FromNamedRecord (..), Header,
                                        decodeByName, (.:))
import qualified Data.Map.Lazy        as Map
import           Data.Maybe           (isJust)
import qualified Data.Text            as T
import qualified Data.Vector          as V
import qualified System.Directory     as Dir
import qualified Data.Set             as Set
import           Data.Foldable        (minimumBy)
import           Data.Function        (on) 

import           Path                 (Node, Dist, Path(..), Coord(..), Network, Link(..), OD(..), Lat, Lon, grtCirDist, Graph(..), Cost(..), shortestPath)



type Org = Node
type Dest = Node

type Highway = Maybe T.Text
type Bridge = Maybe T.Text
type Width = Maybe T.Text

data LinkCsvOut = LinkCsvOut Org Dest Dist deriving (Show)

instance FromNamedRecord LinkCsvOut where
  parseNamedRecord m =
    LinkCsvOut
      <$> m .: "node_id_org"
      <*> m .: "node_id_dest"
      <*> m .: "distance"



type LinkCsv = V.Vector LinkCsvOut

decodeLinkCsv :: FilePath -> IO LinkCsv
decodeLinkCsv fp = do
  cd <- Dir.getCurrentDirectory
  bs <- B.readFile (cd <> fp)
  let Right (_, ls) = decodeByName bs :: Either String (Header, LinkCsv)
  return ls



data NodeCsvOut = NodeCsvOut Node Lat Lon deriving Show

instance FromNamedRecord NodeCsvOut where
  parseNamedRecord m =
    NodeCsvOut
      <$> m .: "node_id"
      <*> m .: "lat"
      <*> m .: "lon"

decodeNodeCsv :: FilePath -> IO NodeCsv
decodeNodeCsv fp = do
  cd <- Dir.getCurrentDirectory
  bs <- B.readFile (cd <> fp)
  let Right (_, ls) = decodeByName bs :: Either String (Header, V.Vector NodeCsvOut)
  return $ makeNodeCsv ls



type NodeCsv =
  Map.Map Node Coord

makeNodeCsv :: V.Vector NodeCsvOut -> NodeCsv
makeNodeCsv = foldr f Map.empty
  where
    f (NodeCsvOut n lat lon) = Map.insert n $ Coord lat lon



makeNetwork :: OD -> NodeCsv -> LinkCsv -> Network
makeNetwork od@(OD c1 c2) nc = foldr f Set.empty
  where
    f (LinkCsvOut org dest dist) = Set.insert $ Path (Cost co dist cd) (Edge (org :->: dest))
      where
        co = grtCirDist c1 (nc Map.! org)
        cd = grtCirDist (nc Map.! dest) c2

nearestNode :: Coord -> NodeCsv -> Node
nearestNode c (Map.assocs -> ncs) = fst . minimumBy (compare `on` snd) $ fmap (grtCirDist c) <$> ncs

makeLink :: OD -> NodeCsv -> Link
makeLink (OD c1 c2) nc = ((:->:) `on` (`nearestNode` nc)) c1 c2

shortestPathCSV :: OD -> NodeCsv -> LinkCsv -> Path
shortestPathCSV od nc lc = shortestPath (makeLink od nc) $ makeNetwork od nc lc