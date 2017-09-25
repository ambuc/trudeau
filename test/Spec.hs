import Graphs

import Test.HUnit

import qualified Data.Maybe as B (isJust)

isomorphismTest = TestList 
                $ map (\n -> TestCase
                           $ assertEqual (m n)
                             (B.isJust $ mkIsomorphism (g n) (h n)) 
                             True
                      ) [2,3,6,9]
          where m n = "Testing isomorphisms between graphs of size " ++ show n
                g n = [1..n] # [ (n,i) | i <- [1..pred n] ]
                h n = [1..n] # [ (1,i) | i <- [2..     n] ]

degreesTest = TestList [ TestCase 
                         $ assertEqual "A vertex of K_153 has degree 152" 
                           (degree (k_ 15) 1) 
                           14
                       , TestCase 
                         $ assertEqual "Testing an example degree distribution" 
                           (degDist $ k_ 5) 
                           (replicate 5 4)
                       ]

planarityTest = TestList
              [ TestCase $ assertEqual "Testing planarity of k_ 4"
                           (isPlanar $ k_ 4)
                           True
              , TestCase $ assertEqual "Testing planarity of k_ 5"
                           (isPlanar $ k_ 5)
                           False
              ]

subgraphTest = TestCase $ assertEqual "Testing a small subgraph"
                          ( f `isSubgraphOf` g )
                          True
  where f = [1,2  ] # [(1,2)      ]
        g = [1,2,3] # [(1,2),(1,3)]

supergraphTest = TestCase $ assertEqual "Testing a small supergraph"
                            ( f `isSupergraphOf` g )
                            True
  where f = [1,2,3] # [(1,2),(1,3)]
        g = [1,2  ] # [(1,2)      ]

expansionTest = TestCase $ assertEqual "Testing a small expansion"
                           ( f `isExpansionOf` g )
                           True
  where f = [1,2,3,4] # [(1,4),(4,2),(2,3),(3,1)]
        g = [1,2,3  ] # [(1   ,   2),(2,3),(3,1)]

main :: IO Counts
main = do
  runTestTT isomorphismTest
  runTestTT     degreesTest
  runTestTT   planarityTest
  runTestTT    subgraphTest
  runTestTT  supergraphTest
  runTestTT   expansionTest
