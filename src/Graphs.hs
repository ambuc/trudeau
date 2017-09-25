module Graphs where

import qualified Data.List   as L 
import qualified Data.Set    as S 
import qualified Data.Maybe  as B 
import qualified Data.Matrix as X 
import qualified Data.Vector as V

-- Chapter 2. Graphs

-- Definition 1. A set is a collection of distict objects, none of which is the
--               set itself

-- Definition 2. A set containing no elements is called a null set or an empty
--               set

-- Definition 3. A set A is said to be a subset  of set B, denoted "A ⊂ B"
--             , if every element of A is also an element of B.
--
-- S.isSubsetOf

-- Definition 4. A set A is said to be equal to a set B, denoted "A = B"
--             , if A ⊂ B and B ⊂ A.

-- Definition 5. A graph is an object consisting of two sets called its vertex 
--               set and its edge set. The vertex set is a finite nonempty set. 
--               The edge set may be empty, but otherwise its elements are 
--               two-element subsets of the vertex set.

type AdjMat = X.Matrix Int
type PerMat = X.Matrix Int
data Graph = Graph { vertices :: S.Set Int
                   , edges    :: AdjMat
                   }

instance (Show Graph) where
  show Graph { vertices = vs
              , edges    = es
              }
       = show es

mkAdjMat :: Int -> [(Int,Int)] -> AdjMat
mkAdjMat w ls = foldr 
                ( (\(i,j) m -> X.unsafeSet 1 (i,j) 
                             $ X.unsafeSet 1 (j,i) m
                  ) . (
                   \(i,j) -> if i>j then (j,i) else (i,j)
                  )
                )
                (X.zero w w)
                $ filter ( uncurry (/=) ) ls

(#) :: [Int] -> [(Int,Int)] -> Graph
vs # es = Graph { vertices = vs'
                , edges    = mkAdjMat width es
                }
  where vs'   = S.fromList vs
        width = S.findMax vs'

toGraph = (#) -- toGraph vs es == vs # es

listVertices g = S.toList $ vertices g

listEdges g 
  | S.null (vertices g) = []
  | otherwise           = map snd 
                        $ filter ((/=0) . fst) 
                        $ filter ((\(i,j) -> j > i).snd)
                        $ zip (X.toList $ edges g) 
                          [(i,j) | i <- range, j <- range]
  where range = [1..S.findMax $ vertices g]
 
-- Definition 6. The elements of the vertex set of a graph are called vertices
--               (singular: vertex) and the elements of the edge set are called 
--               edges.

nVertices = S.size . vertices

nEdges    = length . listEdges

---- Definition 7. If {X,Y} is the edge of a graph, we say that {X,Y} joins or
----               connects the vertices X and Y, and that X and Y are adjacent
----               to each other. The edge {X,Y} is incident to each of X and Y,
--                 and each of X and Y is incident to {X,Y}. Two edges incident 
--                 to the same vertex are called adjacent edges. A vertex 
--                 incident to no edges is isolated.

areAdjacent :: Graph -> Int -> Int -> Bool
areAdjacent g a b
  | S.notMember a ( vertices g ) = error "first vertex not in graph"
  | S.notMember b ( vertices g ) = error "second vertex not in graph"
  | otherwise                    = (==1) $ edges g X.! (a,b)
 
edgesIncident :: Graph -> Int -> [(Int,Int)]
edgesIncident g x = filter (\(i,j) -> j>i)
                  $ map snd $ filter ((==1).fst) $ vBefore ++ vAfter
  where vAfter  = zip (V.toList $ X.getRow x $ edges g) [(x,j) | j <- [1..]]
        vBefore = zip (V.toList $ X.getCol x $ edges g) [(i,x) | i <- [1..]]
 
isIsolated g x = null $ edgesIncident x g
 
-- Definition 8. We will say that two graphs are equal if they have equal vertex
--               sets and equal edge sets. And we will say that two graph 
--               diagrams are equal if they represent equal vertex sets and 
--               equal edge sets.
 
-- Definition 9. If v is an integer greater than or equal to 3, the  cyclic
--               graph on v vertices, denoted C_v, is the graph having vertex
--               set {1,2,3...v} and edge set { {1,2}, {2,3}, {3,4}, ...
--               {v-1,v}, {v,1} }.

c_ :: Int -> Graph
c_ v = [1..v] # take v (zip (cycle [1..v]) (drop 1 $ cycle [1..v]))
cyclicGraph = c_

-- Definition 10 . If v is a positive integer, the null graph on v vertices,
--                 denoted N_v, is the graph having vertex set {1,2,3..v} and no
--                 edges.

n_ :: Int -> Graph
n_ v = [1..v] # []
nullGraph = n_

-- Definition 11. If v is a positive integer, the complete graph on v vertices,
--                denoted K_v, is the graph having vertex set {1,2,3..v} and all
--                possible edges

k_ :: Int -> Graph
k_ v = [1..v] # [ (i,j) | i <- [1..v] , j <- [1..v] , i < j ]
completeGraph = k_

-- Definition 12. The utility graph, denoted UG, is the graph having vertex
--                set {A,B,C,X,Y,Z}, and edge set {{A,X},{A,Y},{A,Z},{B,X},
--                {B,Y},{B,Z},{C,X},{C,Y},{C,Z}}

ug_ :: Graph
ug_ = [1..6] # [ (i,j) | i <- [1..3], j <- [4..6] ]
utilityGraph = ug_

-- Definition 13. If G is a graph then  the complement of G, denoted ~G, is a
--                graph having the same vertex set; the edge set of ~G consists
--                of all two-element subsets of the vertex set which have not
--                already been included  in the edge set of G.

complement g = vs' # es'
  where vs' = S.toList $ vertices g
        es' = filter ( (==0) . (edges g X.!) )
            $ filter ( uncurry (/=) ) -- don't invert diagonal
              [(i,j) | i <- range, j <- range]
        range  = [1..S.findMax $ vertices g]

-- Definition 14. A graph H is a subgraph of a graph G if the vertex set of H is
--                a subset of the vertex set of G and the edge set of H is a
--                subset of the edge set of G

isSubgraphOf :: Graph -> Graph -> Bool
a `isSubgraphOf` b = ( vertices a `S.isSubsetOf` vertices b) 
                  && all (==1) ( map ( edges b X.! ) $ listEdges a )
 
-- Definition 15. If A and B are sets, then  a one-to-one correspondence
--                between A and B is an association of elements of A of elements
--                of B in such a way that:
--                  1) to each element of A there has been associated a single
--                     element of B, and
--                  2) to each element of B there has been associated a single
--                     element of A.

-- Definition 16.   Two graphs are said to be isomorphic if there exists between
--                their vertex sets a one-to-one correspondence having the
--                property that whenever two vertices are adjacent in either
--                graph, the corresponding two vertices are adjacent in the
--                other graph.
--                  Such a one-to-one correspondence is called an isomorphism.
--                  If G and H are isomorphic graphs we denote this by writing 
--                G~=H, or <  G `isIsomorphicTo` H > .
 
mkIsomorphism :: Graph -> Graph -> Maybe PerMat
mkIsomorphism g h
  | X.nrows (edges g) /= X.nrows (edges h) = Nothing
  | X.ncols (edges g) /= X.ncols (edges h) = Nothing
  | nVertices g /= nVertices h             = Nothing
  | degDist g /= degDist h = Nothing
  -- | later we can compare # components too
  | otherwise = L.find (\p -> p * edges g * X.transpose p == edges h) 
              $ allM (X.nrows $ edges g)
  where
    -- returns all permutation matrices of size n
    allM :: Int -> [PerMat]
    allM n = foldr1 (\i j -> (*) <$> i <*> j) $ map (p' n) [1..n]
      where p' m x = map (X.permMatrix m x) [1..x]
 
isIsomorphicTo :: Graph -> Graph -> Bool
isIsomorphicTo g h = B.isJust $ mkIsomorphism g h

checkIsomorphism :: PerMat -> Graph -> Graph -> Bool
checkIsomorphism p g h = (p * edges g * X.transpose p) == edges h
 
g ~= h = isIsomorphicTo g h
 
-- Definition 17. The degree of a vertex is the number of edges incident to it.

degree :: Graph -> Int -> Int
degree g v = length $ edgesIncident g v
 
degDist :: Graph -> [Int]
degDist g = L.sort $ map (degree g) $ S.toList $ vertices g
 
-- From the Exercizes
 
p_ :: Int -> Graph
p_ v = [1..v] # zip [1..pred v] [2..v]
pathGraph = p_
 
w_ :: Int -> Graph
w_ v = [1..v] # ( zip [2..v] (repeat 1) 
                       ++ zip [2..v-1] [3..v] 
                       ++ [(v,2)] 
                )
wheelGraph = w_

-- Chapter 3. Planar Graphs

addE :: (Int, Int) -> Graph -> Graph
addE (i,j) g = Graph { vertices = S.insert i $ S.insert j $ vertices g
                     , edges    = X.unsafeSet 1 (i,j)
                                $ X.unsafeSet 1 (j,i)
                                $ edges g
                     }

--delE doesn't care if the edge was there before or not.
delE :: (Int, Int) -> Graph -> Graph
delE (i,j) g = Graph { vertices = vertices g
                     , edges    = X.unsafeSet 0 (i,j) 
                                $ X.unsafeSet 0 (j,i) 
                                $ edges g
                     }

addV :: Int -> Graph -> Graph
addV v g = Graph { vertices = S.insert v $ vertices g
                 , edges    = edges g
                 }

delV :: Int -> Graph -> Graph
delV v g = Graph { vertices = S.delete v $ vertices g
                 , edges    = X.mapRow (\_ _ -> 0) v
                            $ X.mapCol (\_ _ -> 0) v
                            $ edges g
                 }

contractE :: (Int, Int) -> Graph -> Maybe Graph
contractE (x,y) g 
  | S.null vs' = Nothing
  | otherwise  = Just Graph { vertices = vs'
                            , edges    = es'
                            }
  where vs' = S.delete y $ vertices g
        es' = mkAdjMat (S.findMax vs') ls
        ls  = map  ( (\(i,j) -> if i==y then (x,j) else (i,j))
                   . (\(i,j) -> if j==y then (i,x) else (i,j))
                   ) ( listEdges g )
 
 
minorGraphs :: Graph -> [Graph]
minorGraphs g = L.nubBy (~=)
              $ map (`delE`      g) (listEdges    g)
             ++ map (`delV`      g) (listVertices g)
             ++ B.mapMaybe (`contractE` g) (listEdges    g)
 
-- Definition 18. A graph is planar if it is isomorphic to a graph that has been
--                drawn in a plane without edge-crossings. Otherwise a graph is
--                non-planar

isPlanar :: Graph -> Bool
isPlanar g 
  | g ~= ug_   = False
  | g ~= k_ 5  = False
  | otherwise  = all isPlanar $ minorGraphs g

-- Definition 19. If a graph H is a subgraph of a graph G, we will also say that
--                G is a supergraph of H

isSupergraphOf :: Graph -> Graph -> Bool
a `isSupergraphOf` b = b `isSubgraphOf` a

-- Definition 20. If some new vertices of degree 2 are added to some of the
--                edges of a graph G, the resulting graph H is called an
--                expansion of G.

isExpansionOf :: Graph -> Graph -> Bool
a `isExpansionOf` b = any (~=b) $ B.mapMaybe (`contractE` a) (listEdges a)

makeAllExpansionsOf :: Graph -> [Graph]
makeAllExpansionsOf g = L.nubBy (~=) $ map (vs' #) ess'
  where v    = succ $ S.findMax $ vertices g
        vs'  = S.toList $ S.insert v $ vertices g
        ess' = map (\(i,j) -> (i,v) : (v,j) : L.delete (i,j) (listEdges g)
                   ) (listEdges g)

-- Chapter 4. Euler's Formula

-- Definition 21. A walk in a graph is a sequence {A1, A2, A3... An} of not
--                necessarily distinct vertices in which A1 is joined by an edge
--                to A2, A2 is joined by an edge to A3,... and An-1 is jooined
--                by an edge to An. The walk {A1, A2, A3... An} is said to join
--                A1 and An.

isWalk :: Graph -> [Int] -> Bool
isWalk g [x]       = True
isWalk g xs@(x:y:_) = areAdjacent g x y && isWalk g (drop 1 xs)

-- Definition 22. A graph is said to be connected if every pair of vertices is
--                joined by a walk. Otherwise a graph is said to be
--                disconnected.

-- TODO: implement 
-- isConnected :: Graph a -> Bool

--  Definition 23. When a planar graph is actually drawn in a plane without
--                 edge-crossings, it cuts the plane into regions called faces
--                 of the g raph. The letter `f` shall denote the number of
--                 faces of a planar graph.

-- Definition 24. A graph is polygonal if it is planar, connected, and has the
--                property that every edge borders on two different faces.
