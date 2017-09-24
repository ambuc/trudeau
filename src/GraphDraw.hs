module GraphDraw where

import Graphs

import Data.Set as S  (toList)
import System.IO      (IOMode (..), openFile, hPutStrLn, hClose, hGetContents)
import System.Process (StdStream (..), createProcess, proc, std_out)
import Data.Char      (toLower)

data DotProgram = Twopi | Circo | Dot | Neato | Fdp | Sfdp deriving (Show)

-- Example usage:
-- > graphDraw "/tmp/graph.png" (k_ 20) 
graphDraw :: (Eq a, Ord a, Show a) => FilePath -> DotProgram -> Graph a -> IO ()
graphDraw fp pg g = do
  -- write dotfile
  let tmp = "/tmp/dot"
  h <- openFile tmp WriteMode
  hPutStrLn h "graph {"
  hPutStrLn h "\tnode[label=\"\",shape=point];"
  mapM_ ( hPutStrLn h 
        . (\i -> "\t\"" ++ show i ++ "\"")
        ) $ S.toList $ vertices g
  mapM_ ( hPutStrLn h 
        . (\(i,j) -> "\t\"" ++ show i ++ "\" -- \"" ++ show j ++ "\"")
        ) $ S.toList $ edges g
  hPutStrLn h "}"
  hClose h
  -- run whatever program on it
  let dp = map toLower $ show pg
  (_, Just hout, _, _) <- createProcess 
      ( proc dp ["-Tpng", tmp, "-o", fp] )
      { std_out = CreatePipe }
  out <- hGetContents hout
  putStrLn out


