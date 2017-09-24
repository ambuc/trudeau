import Graphs

import Test.HUnit

import qualified Data.Set  as S (Set, empty, toList)
import qualified Data.List as L (permutations)

-- Theorem 1. There is only one empty set.
theorem01 = TestCase $ assertEqual "There is only one empty set" 
            (S.empty :: S.Set Int) 
            (S.empty :: S.Set Int)

-- Theorem 2. The number of edges in a complete graph K_v is given by the
--            formula e = (1/2) * v * (v-1)
theorem02 = TestList 
          $ map (\n -> TestCase 
                     $ assertEqual ("Trial for " ++ show n ++ " edges") 
                       (nEdges $ completeGraph n) 
                       (div (n * (n-1)) 2) 
                ) [1,5,20,37,101]

-- Theorem 4. K_5 is nonplanar.
theorem04 = TestCase $ assertEqual "K_5 is nonplanar" 
                       False 
                       (isPlanar $ k_ 5)

-- Theorem 5. Any subgraph of a planar graph is planar.
theorem05 = TestList
          [ TestCase $ assertEqual "K_4 is planar"
                       True
                       (isPlanar $ k_ 4)
          , TestCase $ assertEqual "and so are all its subgraphs"
                       True
                       (all isPlanar $ minorGraphs $ k_ 4)
          ]

-- Theorem 6. Every expansion of UG or K_5 is nonplanar.

-- Example21-02. Any sequence of distinct vertices in k_v is a walk.
e21x02 = TestList
       $ map (\n -> TestCase
                  $ assertEqual "Any seq. of distinct vertices in k_v is a walk"
                    True
                    ( all (isWalk $ k_ n) $ L.permutations 
                    $ S.toList $ vertices $ k_ n
                    )
             ) [2,3,5]

-- Theorem 8. If a graph g is polygonal then v + f - e = 2
-- Theorem 9. If a graph g is planar and connected but not polygonal,
--            then v + f - e = 2
-- Theorem 11. If G is planar and connected with v>=3, then (3/2)f <= e <= 3v-6
-- Theorem 12. If G is planar and connected with v>=3 and G is not a supergraph
--             of k_3, then 2f <= e <= 2v-4
-- Theorem 13. If G is planar and connected then G has a vertex of degree <= 5


main :: IO Counts
main = do
  runTestTT theorem01
  runTestTT theorem02
  runTestTT theorem04
  runTestTT theorem05
  runTestTT e21x02
