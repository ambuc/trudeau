module Graphs where

import qualified Data.List   as L 
import qualified Data.Set    as S 
import qualified Data.Maybe  as B 
import qualified Data.Matrix as X 

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

data Graph a = Graph (S.Set a) (S.Set (a,a)) deriving (Show)

instance (Eq a) => Eq (Graph a) where
    Graph v e == Graph v' e' = (e == e') && (v == v')

-- for set inputs
(%) :: (Ord a, Eq a) => S.Set a -> S.Set (a,a) -> Graph a
vs % es = if all (==True) $ S.map (\(i,j) -> S.member i vs && S.member j vs) es'
            then Graph vs es'
            else error "Edges contain vertices not mentioned in definition"
  where es' = S.filter ( uncurry (/=) )
            $ S.map    ( \(i,j) -> if j > i then (i,j) else (j,i) ) 
              es

-- for list inputs, we're happy to turn them into sets for you
(#) :: (Ord a, Eq a) => [a] -> [(a,a)] -> Graph a
vs # es = S.fromList vs % S.fromList es

vertices :: (Eq a) => Graph a -> S.Set a
vertices (Graph vs _) = vs

edges :: (Eq a) => Graph a -> S.Set (a,a)
edges (Graph _ es) = es

listVertices :: (Eq a, Ord a) => Graph a -> [a]
listVertices = S.toList . vertices

listEdges :: (Eq a, Ord a) => Graph a -> [(a,a)]
listEdges = S.toList . edges

-- Definition 6. The elements of the vertex set of a graph are called vertices
--               (singular: vertex) and the elements of the edge set are called 
--               edges.

nVertices :: (Ord a, Eq a) => Graph a -> Int
nVertices = S.size . vertices

nEdges :: (Ord a, Eq a) => Graph a -> Int
nEdges = S.size . edges

---- Definition 7. If {X,Y} is the edge of a graph, we say that {X,Y} joins or
----               connects the vertices X and Y, and that X and Y are adjacent
----               to each other. The edge {X,Y} is incident to each of X and Y,
--                 and each of X and Y is incident to {X,Y}. Two edges incident 
--                 to the same vertex are called adjacent edges. A vertex 
--                 incident to no edges is isolated.
areAdjacent :: (Ord a, Eq a) => a -> a -> Graph a -> Bool
areAdjacent a b g
  | a > b     = areAdjacent b a g
  | otherwise = (a,b) `S.member` edges g

edgesIncident :: (Ord a, Eq a) => a -> Graph a -> S.Set (a,a)
edgesIncident v g = S.filter (\(i,j) -> (v==i) || (v==j)) $ edges g

isIsolated :: (Ord a, Eq a) => a -> Graph a -> Bool
isIsolated v g = S.null $ S.filter (\(i,j) -> (v==i) || (v==j)) $ edges g

-- Definition 8. We will say that two graphs are equal if they have equal vertex
--               sets and equal edge sets. And we will say that two graph 
--               diagrams are equal if they represent equal vertex sets and 
--               equal edge sets.

-- (==) already works.

-- Definition 9. If v is an integer greater than or equal to 3, the  cyclic
--               graph on v vertices, denoted C_v, is the graph having vertex
--               set {1,2,3...v} and edge set { {1,2}, {2,3}, {3,4}, ...
--               {v-1,v}, {v,1} }.

cyclicGraph :: Int -> Graph Int
cyclicGraph v = [1..v] # take v (zip (cycle [1..v]) (drop 1 $ cycle [1..v]))

c_ = cyclicGraph

-- Definition 10 . If v is a positive integer, the null graph on v vertices,
--                 denoted N_v, is the graph having vertex set {1,2,3..v} and no
--                 edges.

nullGraph :: Int -> Graph Int
nullGraph v = [1..v] # []

n_ = nullGraph

-- Definition 11. If v is a positive integer, the complete graph on v vertices,
--                denoted K_v, is the graph having vertex set {1,2,3..v} and all
--                possible edges

completeGraph :: Int -> Graph Int
completeGraph v = [1..v] # [ (i,j) | i <- [1..v] , j <- [1..v] , i < j ]

k_ = completeGraph

-- Definition 12. The utility graph, denoted UG, is the graph having vertex
--                set {A,B,C,X,Y,Z}, and edge set {{A,X},{A,Y},{A,Z},{B,X},
--                {B,Y},{B,Z},{C,X},{C,Y},{C,Z}}

utilityGraph :: Graph Int
utilityGraph = [1..6] # [ (i,j) | i <- [1..3], j <- [4..6] ]

ug_ = utilityGraph

-- Definition 13. If G is a graph then  the complement of G, denoted ~G, is a
--                graph having the same vertex set; the edge set of ~G consists
--                of all two-element subsets of the vertex set which have not
--                already been included  in the edge set of G.

complement :: (Ord a, Eq a, Num a) => Graph a -> Graph a
complement g = vs' % es'
  where vs' = vertices g
        es' = S.fromList [ (i,j) | i <- vs_list
                         , j <- vs_list
                         , i < j
                         , (i,j) `S.notMember` edges g
                         ]
        vs_list = S.elems $ vertices g

-- Definition 14. A graph H is a subgraph of a graph G if the vertex set of H is
--                a subset of the vertex set of G and the edge set of H is a
--                subset of the edge set of G

isSubgraphOf :: (Eq a, Ord a) => Graph a -> Graph a -> Bool
isSubgraphOf a b = (   edges a `S.isSubsetOf`    edges b) 
                && (vertices a `S.isSubsetOf` vertices b)

-- Definition 15. If A and B are sets, then  a one-to-one correspondence
--                between A and B is an association of elements of A of elements
--                of B in such a way that:
--                  1) to each element of A there has been associated a single
--                     element of B, and
--                  2) to each element of B there has been associated a single
--                     element of A.

existsCorrespondence :: (Eq a, Ord a) => S.Set a -> S.Set a -> Bool
existsCorrespondence a b = S.size a == S.size b

-- Definition 16.   Two graphs are said to be isomorphic if there exists between
--                their vertex sets a one-to-one correspondence having the
--                property that whenever two vertices are adjacent in either
--                graph, the corresponding two vertices are adjacent in the
--                other graph.
--                  Such a one-to-one correspondence is called an isomorphism.
--                  If G and H are isomorphic graphs we denote this by writing 
--                G~=H, or <  G `isIsomorphicTo` H > .

isIsomorphicTo :: (Eq a, Ord a, Eq b, Ord b) => Graph a -> Graph b -> Bool
isIsomorphicTo g h = B.isJust $ mkIsomorphism g h

g ~= h = isIsomorphicTo g h

mkIsomorphism :: (Ord a, Ord b) => Graph a -> Graph b -> Maybe [(a,b)]
mkIsomorphism g h 
  | nEdges    g /= nEdges    h = Nothing
  | nVertices g /= nVertices h = Nothing
  | degDist   g /= degDist   h = Nothing
  -- | later we can compare number of components
  | otherwise                  = L.find (isIsomorphism g h) 
                               $ map (zip (listVertices g))
                               $ L.permutations -- extremely inefficient
                               $ listVertices h

isIsomorphism :: (Eq a, Ord b) => Graph a -> Graph b -> [(a,b)] -> Bool
isIsomorphism g h m = relabel g m == h
  where relabel :: (Eq a, Ord b) => Graph a -> [(a,b)] -> Graph b
        relabel g m' = vs % es
          where vs = S.map (\ i    ->   B.fromJust $ lookup i m' ) $ vertices g
                es = S.map (\(i,j) -> ( B.fromJust $ lookup i m'
                                      , B.fromJust $ lookup j m')) $ edges    g

-- Definition 17. The degree of a vertex is the number of edges incident to it.

degree :: (Eq a, Ord a) => Graph a -> a -> Int
degree g v = S.size $ S.filter (\(i,j) -> i == v || j == v) $ edges g

degDist :: (Eq a, Ord a) => Graph a -> [Int]
degDist g = L.sort $ map (degree g) $ S.toList $ vertices g

-- From the Exercizes

pathGraph :: Int -> Graph Int
pathGraph v = [1..v] # zip [1..pred v] [2..v]

p_ = pathGraph

wheelGraph :: Int -> Graph Int
wheelGraph v = [1..v] # ( zip [2..v] (repeat 1) 
                       ++ zip [2..v-1] [3..v] 
                       ++ [(v,2)] 
                        )

w_ = wheelGraph

-- Chapter 3. Planar Graphs

removeEdge :: (Ord a, Eq a) => (a,a) -> Graph a -> Maybe (Graph a)
removeEdge e g
  | S.member e (edges g) = Just $ vertices g % S.delete e (edges g)
  | otherwise            = Nothing

removeVertex :: (Ord a, Eq a) => a -> Graph a -> Maybe (Graph a)
removeVertex v g 
  | S.member v (vertices g) = Just $ vs' % es'
  | otherwise               = Nothing
  where vs' = S.delete v $ vertices g
        es' = S.filter (\(i,j) -> i /= v && j /= v) $ edges g

contractEdge :: (Ord a, Eq a) => (a,a) -> Graph a -> Maybe (Graph a)
contractEdge e@(x,y) g 
  | S.member e (edges g) = Just $ vs' % es'
  | otherwise            = Nothing
  where vs' = S.delete y $ vertices g
        es' = S.filter (uncurry (/=))
            $ S.map    (\(i,j) -> if i==y then (x,j) else (i,j))
            $ S.map    (\(i,j) -> if j==y then (i,x) else (i,j))
            $ edges g

minorGraphs :: (Ord a, Eq a) => Graph a -> [ Graph a ]
minorGraphs g = L.nubBy (~=)
              $ B.mapMaybe (`removeEdge`   g) (listEdges    g)
             ++ B.mapMaybe (`removeVertex` g) (listVertices g)
             ++ B.mapMaybe (`contractEdge` g) (listEdges    g)

-- Definition 18. A graph is planar if it is isomorphic to a graph that has been
--                drawn in a plane without edge-crossings. Otherwise a graph is
--                non-planar

isPlanar :: (Ord a, Eq a) => Graph a -> Bool
isPlanar g 
  | g ~= ug_   = False
  | g ~= k_ 5  = False
  | otherwise  = all isPlanar $ minorGraphs g

-- Definition 19. If a graph H is a subgraph of a graph G, we will also say that
--                G is a supergraph of H

isSupergraphOf :: (Eq a, Ord a) => Graph a -> Graph a -> Bool
a `isSupergraphOf` b = b `isSubgraphOf` a

-- Definition 20. If some new vertices of degree 2 are added to some of the
--                edges of a graph G, the resulting graph H is called an
--                expansion of G.

isExpansionOf :: (Eq a, Ord a) => Graph a -> Graph a -> Bool
a `isExpansionOf` b = elem b $ B.mapMaybe (`contractEdge` a) (listEdges a)

makeAllExpansionsOf :: (Enum a, Eq a, Ord  a) => Graph a -> [Graph a]
makeAllExpansionsOf g = L.nubBy (~=) $ map (vs' %) ess'
  where v = succ $ S.findMax $ vertices g
        vs' = S.insert v $ vertices g
        ess' = map (\(i,j) -> S.insert (i,v) $ S.insert (v,j) 
             $ S.delete (i,j) (edges g)) 
             $ listEdges g

-- Chapter 4. Euler's Formula

-- Definition 21. A walk in a graph is a sequence {A1, A2, A3... An} of not
--                necessarily distinct vertices in which A1 is joined by an edge
--                to A2, A2 is joined by an edge to A3,... and An-1 is jooined
--                by an edge to An. The walk {A1, A2, A3... An} is said to join
--                A1 and An.

isWalk :: (Ord a, Eq a) => Graph a -> [a] -> Bool
isWalk g [x]       = True
isWalk g xs@(x:y:_) = areAdjacent x y g && isWalk g (drop 1 xs)

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
