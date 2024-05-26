module Graphs where

import Numeric

-- We assume that the vertices are numbered 0 ... n-1

{- In and adjacency list al, the i-th element al!!i
   is a list of all the vertices j such that
   there is an edge from i to j
-}

type AdjList = [[Int]]

{- In and adjacencly matrix am, the element with
   coordinates i,j, that is am!!i!!j
   is True if there is an edge from i to j
      False if there is no edge from i to j
-}

type AdjMatrix = [[Bool]]

-- Suppose we're given a graph as a list of edges (i,j)
-- Generate the Adjacency List and Adjacencty Matrix representations

-- GENERATION OF ADJACENCY LIST

adjList :: [(Int, Int)] -> AdjList
adjList [] = []
adjList edges = buildAdjList n edges
  where
    n = findMaxInTuples edges -- The adjacency list will contain as many entries as the max element in the input list 

    buildAdjList :: Int -> [(Int, Int)] -> AdjList
    buildAdjList n edges = [ [j | (i, j) <- edges, i == index] | index <- [0..n] ]  -- For all edges, when there is an edge from i to j, add j to the list at index i

-- This function finds the max element across the list across all tuples
findMaxInTuples :: (Ord a) => [(a, a)] -> a
findMaxInTuples [] = error "Empty list"
findMaxInTuples ((x,y):xs) = maxHelper xs (max x y) -- Set the max of the 2 elements in the first tuple as the currentMax
   where
      maxHelper [] currentMax = currentMax
      maxHelper ((a,b):rest) currentMax = maxHelper rest (max (max a b) currentMax) -- Recursively find the max by comparing max of each tuple with currentMax


-- GENERATION OF ADJACENCY MATRIX

adjMatrix :: [(Int,Int)] -> AdjMatrix
adjMatrix [] = []
adjMatrix edges = buildAdjMatrix n edges
   where
      n = findMaxInTuples edges

      buildAdjMatrix :: Int -> [(Int, Int)] -> AdjMatrix
      -- For each position (rowIndex, colIndex) check if it is an element of the edges list
      buildAdjMatrix n edges = [ [(rowIndex, colIndex) `elem` edges | colIndex <- [0..n]] | rowIndex <- [0..n] ] 

--------------------------------------------------------

-- -- WEIGHTED GRAPHS: every edge has a "weight"

-- {- In an adjacency list al, the i-th element al!!i
--    contains all the pairs (j,w) such that
--    there is an edge from i to j with weight w
-- -}

-- type WAdjList = [[(Int,Float)]]

-- {- In an adjacency matrix am, the element with
--    coordinates i,j is
--      Nothing if there is no edge from i to j
--      (Just w) if there is an edge from i to j with weight w
-- -}

-- type WAdjMatrix = [[Maybe Float]]

-- {- We can also represent a weighted graphs by a list of edges
--    (i,j,w) denotes an edge from i to j with weight w
-- -}

-- type Edges = [(Int,Int,Float)]

-- -- GENERATION OF ADJACENCY LIST
-- --   from a list of edges

-- adjListW :: Edges -> WAdjList

-- -- GENERATION OF ADJACENCY MATRIX
-- --   from a list of edges

-- adjMatrixW :: Edges -> WAdjMatrix

-- -- DIJKSTRA'S ALGORITHM

-- {- 
--    Given an adjacencly list al and a source vertex s
--    return the list of minimum distances of vertices from s:
--    (dijkstra al s)!!j is the minimum distance from s to j
-- -}

-- dijkstra :: WAdjList -> Int -> [Maybe Float]

-- -- FLOYD-WARSHALL ALGORITHM

-- {-
--    Given an adjacency matrix am, return the matrix of minimum distances:
--    (floydWarshall am)!!i!!j is
--      Nothing if there is no path from i to j
--      (Just x) if the shortest path from i to j has length x
-- -}

-- floydWarshall :: WAdjMatrix -> WAdjMatrix

