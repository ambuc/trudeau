import Graphs

import Test.HUnit

e01x01 = TestCase $ assertEqual "" 
                    8
                    ( length $ (!!2) 
                    $ iterate ( concatMap (\x -> [ True:x, False:x ])
                              ) [ [True],[False] ]
                    )

exercizes = TestList [ e01x01 ]

main :: IO Counts
main = do
  runTestTT exercizes
  runTestTT exercizes
