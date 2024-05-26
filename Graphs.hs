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

-- WEIGHTED GRAPHS: every edge has a "weight"

{- In an adjacency list al, the i-th element al!!i
   contains all the pairs (j,w) such that
   there is an edge from i to j with weight w
-}

type WAdjList = [[(Int,Float)]]

{- In an adjacency matrix am, the element with
   coordinates i,j is
     Nothing if there is no edge from i to j
     (Just w) if there is an edge from i to j with weight w
-}

type WAdjMatrix = [[Maybe Float]]

{- We can also represent a weighted graphs by a list of edges
   (i,j,w) denotes an edge from i to j with weight w
-}

type Edges = [(Int,Int,Float)]

-- GENERATION OF ADJACENCY LIST
--   from a list of edges

adjListW :: Edges -> WAdjList
adjListW [] = []
adjListW weightedEdges = buildAdjListW n weightedEdges
  where
    n = findMaxInWeightedEdges weightedEdges -- The adjacency list will contain as many entries as the max vertice value in the input list 

    buildAdjListW :: Int -> Edges -> WAdjList
    buildAdjListW n weightedEdges = [ [(j,w) | (i, j, w) <- weightedEdges, i == index] | index <- [0..n] ]  -- For all edges, when there is an edge from i to j, add (j,w) to the list at index i

-- This function finds the max vertice value
findMaxInWeightedEdges :: (Ord a) => [(a, a, b)] -> a
findMaxInWeightedEdges [] = error "Empty list"
findMaxInWeightedEdges ((x,y,_):xs) = maxHelper xs (max x y) -- Set the max of the 2 vertice values in the first tuple as the currentMax
   where
      maxHelper [] currentMax = currentMax
      maxHelper ((a,b,_):rest) currentMax = maxHelper rest (max (max a b) currentMax) -- Recursively find the max by comparing max of each tuple with currentMax

-- GENERATION OF ADJACENCY MATRIX
--   from a list of edges

adjMatrixW :: Edges -> WAdjMatrix
adjMatrixW [] = []
adjMatrixW weightedEdges = buildWeightedAdjMatrix n weightedEdges
  where
    n = findMaxInWeightedEdges weightedEdges

    buildWeightedAdjMatrix :: Int -> Edges -> WAdjMatrix
    -- For each position (rowIndex, colIndex), check if it is an element of the edges list and get its weight
    buildWeightedAdjMatrix n edges = 
      [ [ findEdgeWeight (rowIndex, colIndex) edges | colIndex <- [0..n]] | rowIndex <- [0..n] ] 

    -- Function to find the weight of an edge, if it exists
    findEdgeWeight :: (Int, Int) -> [(Int, Int, Float)] -> Maybe Float
    findEdgeWeight _ [] = Nothing
    findEdgeWeight (row, col) ((x, y, w):rest) 
      | row == x && col == y = Just w
      | otherwise = findEdgeWeight (row, col) rest

-- DIJKSTRA'S ALGORITHM

{- 
   Given an adjacencly list al and a source vertex s
   return the list of minimum distances of vertices from s:
   (dijkstra al s)!!j is the minimum distance from s to j
-}

-- PriorityQueue is a list of tuples, each containing a vertex and its distance.
type PriorityQueue = [(Int, Maybe Float)]

-- Insert a vertex with its distance into the priority queue.
insert :: (Int, Maybe Float) -> PriorityQueue -> PriorityQueue
insert item pq = item : pq

-- Extracts the minimum priority element from a priority queue and returns it along with the updated priority queue.
extractMin :: PriorityQueue -> ((Int, Maybe Float), PriorityQueue)
extractMin pq = case removeMin pq Nothing of
                  (Just minItem, _) -> (minItem, filter (/= minItem) pq)
                  (Nothing, _) -> error "Priority Queue is empty"
  where
    -- Helper function to find the minimum element in the priority queue.
    removeMin :: PriorityQueue -> Maybe (Int, Maybe Float) -> (Maybe (Int, Maybe Float), PriorityQueue)
    removeMin [] currentMin = (currentMin, [])
    removeMin (x:xs) Nothing = removeMin xs (Just x)
    removeMin (x:xs) (Just currentMin@(v1, d1))
      | snd x < d1 = removeMin xs (Just x)          -- If the current element's distance is less than the current minimum distance, update the minimum.
      | otherwise = removeMin xs (Just currentMin)  -- Otherwise, continue with the current minimum.


-- Update the distance of a specific vertex in the priority queue.
decreaseKey :: Int -> Maybe Float -> PriorityQueue -> PriorityQueue
decreaseKey vertex newDist pq = map update pq
  where
    update (v, dist)  
      | v == vertex = (v, min newDist dist) -- Update the priority of the specificed vertex
      | otherwise   = (v, dist)

-- Initialize the priority queue with all vertices, setting the source vertex's distance to 0
-- and all others to infinity.
initializePQ :: Int -> Int -> PriorityQueue
initializePQ numVertices source = [(i, if i == source then Just 0 else Just (1/0)) | i <- [0..numVertices - 1]]

-- Initialize the distances for all vertices, setting the source vertex's distance to 0
-- and all others to infinity.
initializeDistances :: Int -> Int -> [Maybe Float]
initializeDistances numVertices source = [if i == source then Just 0 else Just (1/0) | i <- [0..numVertices - 1]]

dijkstra :: WAdjList -> Int -> [Maybe Float]
dijkstra graph source = finalize ( dijkstra' (initializePQ (length graph) source) (initializeDistances (length graph) source) ) -- Num of vertices = length of adj list
  where
    dijkstra' :: PriorityQueue -> [Maybe Float] -> [Maybe Float]
    dijkstra' [] dists = dists
    dijkstra' pq dists = 
      let ((u, Just distU), pq') = extractMin pq                                              -- Extract the minimum priority tuple from the Priority Queue
          neighbors = graph !! u                                                              -- Get the neighbors of current vertex u
      in dijkstra' (foldl (updatePQ distU) pq' neighbors) (updateDists distU dists neighbors) -- Update both the priority queue and the distances

    -- This function updates the Priority Queue using decreaseKey
    updatePQ :: Float -> PriorityQueue -> (Int, Float) -> PriorityQueue
    updatePQ distU pq (v, weight) = decreaseKey v (Just (distU + weight)) pq

    -- This function updates the distances list with the minimum distance
    updateDists :: Float -> [Maybe Float] -> [(Int, Float)] -> [Maybe Float]
    updateDists distU dists neighbors = foldl update dists neighbors
      where
        update dists' (v, weight) = replaceNthDistance v (calculateNewDist v distU weight dists') dists'

    -- Calculate the new distance for a given neighbor
    calculateNewDist :: Int -> Float -> Float -> [Maybe Float] -> Maybe Float
    calculateNewDist v distU weight dists = 
      let oldDist = retreiveMaybe (dists !! v) -- Fetch the old distance of u to v from the distances list
          newDist = distU + weight             -- Calculate the new distance as sum of the distance to reach u and the weight of the edge from u to v
      in Just (min newDist oldDist)            -- Take the minimum of newDist and oldDist

    -- Replaces the distance at a specified index with a new value
    replaceNthDistance :: Int -> Maybe Float -> [Maybe Float] -> [Maybe Float]
    replaceNthDistance targetIndex newValue distancesList = take targetIndex distancesList ++ [newValue] ++ drop (targetIndex + 1) distancesList

    -- Convert Infinity to Nothing, to represent the vertices which have no incoming path
    finalize :: [Maybe Float] -> [Maybe Float]
    finalize = map (\x -> if x == Just (1/0) then Nothing else x)
    
    -- This function returns just the value or infinity depending on the distance (type Mayvbe) 
    retreiveMaybe :: Maybe Float -> Float
    retreiveMaybe (Just x) = x
    retreiveMaybe Nothing = 1/0

-- -- FLOYD-WARSHALL ALGORITHM

-- {-
--    Given an adjacency matrix am, return the matrix of minimum distances:
--    (floydWarshall am)!!i!!j is
--      Nothing if there is no path from i to j
--      (Just x) if the shortest path from i to j has length x
-- -}

-- floydWarshall :: WAdjMatrix -> WAdjMatrix

