import Data.Char
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph
-- import Data.Graph.Analysis.Algorithms.Common

-- Function mapping from current state to next state.
-- E.g. throw "xxxx___" 5 gives "xxx_x__"
throw state n = drop 1 $ before ++ 'x' : after
    where (before, _:after) = splitAt n (state ++ "_")

emptyEdge b h = ([], 0, (replicate b 'x') ++ (replicate (h-b) '_'))
  
-- Graph edges for a given siteswap.
-- List of starting states, (say, xxxx___), throws (e.g. 5), and resulting states (e.g. xxx_x__)
siteswapEdges ss = nub $ take (length ss) $ drop (100 * length ss)
    $ scanl nextEdge (emptyEdge 0 $ maximum ss) (concat . repeat $ ss)
    where nextEdge (startState, _, endState) s = (endState, s, throw endState s)

-- Graph edges for a given number of balls and height.
allEdges b h = Set.toList $ converge edgesSet $ Set.singleton $ emptyEdge b h
  where edgesSet sEdges = Set.fromList $ concatMap edgeToEdges sEdges
        edgeToEdges (_,_,st) = [(st, t, st') | t <- throws st, let st' = throw st t, sort st == sort st']
          where throws (s:ss) | s == '_' = [0] | otherwise = [1..length st]

-- Magic to repeatedly apply a function to itself until the output is stable.
converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)


-- ... Output ...

-- Print table for a given siteswap
printTable ss = putStrLn $ unlines $ map showSt (siteswapEdges ss)
    where showSt (_, s, state) = (show s) ++ "|" ++ state

-- Printing a graph to (roughly) graphviz format.
printGraph edges = putStrLn . unlines $ map showSt edges
  where showSt (st, t, st') = st ++ " -> " ++ st' ++ " [label=\"" ++ (show t) ++ "\"]"

-- main = printGraph $ allEdges 3 5
-- main = printGraph $ concat [allEdges b 9 | b <- [0..9]]
main = printTable $ map digitToInt "97531"
  
-- main = do
--   printTable $ map digitToInt "66671777161" 
--   printTable $ map digitToInt "6316131" 

-- -- Construct a directed graph from some edges.
ssGraph :: [(String, Int, String)] -> Gr String Int
ssGraph grEdges = mkGraph (zip [0..] grVertices) edges
  where
    edges = map (\(st, t, st') -> (stateIx st, stateIx st', t)) grEdges
    grVertices = sort $ nub $ map (\(a,_,_) -> a) grEdges
    stateIx s = fromJust $ elemIndex s grVertices
