module Main where

import qualified System.Directory     as Dir
import           System.Environment

import AStar
import CSV

main :: IO ()
main = do
  args <- getArgs

  let [latOrg, lonOrg, latDest, lonDest] = read <$> args
  let od = OD (Coordinates latOrg lonOrg) (Coordinates latDest lonDest)

  lc <- decodeLinkCsv "/output/simple_links.csv"
  nc <- decodeNodeCsv "/output/simple_nodes.csv"
  
  print . cost $ shortestPathCSV od nc lc

  {-
  cd <- Dir.getCurrentDirectory
  writeFile (cd <> "/output/simple_link.csv") $ encodeLinkCsv slc
  writeFile (cd <> "/output/simple_node.csv") $ encodeNodeCsv snc
  -}
