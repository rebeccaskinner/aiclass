{-# LANGUAGE NoMonomorphismRestriction #-}
import System (getArgs)
import Control.Arrow (second)
import Data.Maybe
import Data.Graph.Inductive
import Data.Graph.Inductive.Graphviz

type ObservableState = (String, Float)

data MarkovNode = MarkovNode { nodeName        :: String
                             , nodeProbability :: Float
                             , nodeStates      :: [ObservableState] } deriving (Eq)

instance Show MarkovNode where
    show n = nodeName n ++ "(" ++ show (nodeProbability n) ++ ")"

-- rainStates = [("H",0.4),("G",0.6)]
-- sunStates = [("H",0.9),("G",0.1)]

aStates = [("x",0.1),("y",0.9)]
bStates = [("x",0.8),("y",0.2)]

stateProbAt name node = let states = nodeStates node in
                        (snd . head . filter ((name==).fst)) states

stateProbIn name node = let states = nodeStates node in
                        nodeProbability node * (snd . head . filter ((name==).fst)) states

testGraph :: Gr MarkovNode Float
testGraph = mkGraph (zip [1..] [MarkovNode "a" 0.5 aStates, MarkovNode "b" 0.5 bStates]) [(1,1,0.5),(1,2,0.5),(2,1,0.5),(2,2,0.5)]
-- testGraph = mkGraph (zip [1..] [MarkovNode "l" 0.5 aStates, MarkovNode "f" 0.5 fair]) [(1,1,0.6),(1,2,0.4),(2,1,0.2),(2,2,0.8)]

getNodesWithLabels g = map (second fromJust) $ filter (isJust . snd) $ zip (nodes g ) $ map (lab g) (nodes g)
findNodeByLabel g l = fst . head $ filter (\x -> l == snd x) (getNodesWithLabels g)
findNodeByName g l = (fst . head) $ filter (\(x,y) -> l == nodeName y) (getNodesWithLabels g)
findLabelByName g l = (fromJust . lab g) $ findNodeByName g l

edgeProbability g (s,_,p) = p * nodeProbability ((fromJust . lab g) s)

updateMarkovNode g mn = let n = findNodeByLabel g mn 
                            prob = sum $ map (edgeProbability g) $ inn g n in
                       MarkovNode (nodeName mn) prob (nodeStates mn)

updateGraph g = nmap (updateMarkovNode g) g

graphTick g 0 = g
graphTick g n = graphTick (updateGraph g) (n - 1)

justNodes g = map fromJust $ filter isJust $ map (lab g) $ nodes g

stateTotalProb name g = sum $ map (stateProbIn name) (justNodes g)

-- given that we know observable, what is the probability of node q
-- e.g. hiddenModelInfer "H" "r" (graphTick testGraph 1)
hiddenModelInfer observable qn g = let q = findLabelByName g qn in
                                   (stateProbAt observable q * nodeProbability q) / stateTotalProb observable g

main = do
    count <- (return . read . head) =<< getArgs
    let g = graphTick testGraph count
    putStrLn $ graphviz g "test_graph" (8.5,11.0) (1,1) Portrait

