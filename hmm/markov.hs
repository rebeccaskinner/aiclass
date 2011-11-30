{-# LANGUAGE NoMonomorphismRestriction #-}
import System (getArgs)
import Data.Maybe
import Data.Graph.Inductive
import Data.Graph.Inductive.Graphviz

type ObservableState = (String, Float)

data MarkovNode = MarkovNode { nodeName :: String, nodeProbability :: Float } deriving (Eq)

instance Show MarkovNode where
    show n = nodeName n ++ "(" ++ show (nodeProbability n) ++ ")"


testGraph :: Gr MarkovNode Float
-- testGraph = mkGraph (zip [1..] [MarkovNode "a" 1.0 , MarkovNode "b" 0.0]) ([(1,1,0.5),(1,2,0.5),(2,1,1.0)])
testGraph = mkGraph (zip [1..] [MarkovNode "a" 0.5 , MarkovNode "b" 0.5]) [(1,1,0.9),(1,2,0.1),(2,1,0.5),(2,2,0.5)]

getNodesWithLabels g = map (\(x,y)->(x,fromJust y)) $ filter (isJust . snd) $ zip (nodes g ) $ map (lab g) (nodes g)
findNodeByLabel g l = fst . head $ filter (\x -> l == (snd x)) (getNodesWithLabels g)
findNodeByName g l = (fst . head) $ filter (\(x,y) -> l == (nodeName y)) (getNodesWithLabels g)
findLabelByName g l = (fromJust . (lab g)) $ findNodeByName g l

edgeProbability g (s,_,p) = p * (nodeProbability ((fromJust . (lab g)) s))

updateMarkovNode g mn = let n = findNodeByLabel g mn 
                            prob = sum $ map (edgeProbability g) $ inn g n in
                       MarkovNode (nodeName mn) prob

updateGraph g = nmap (updateMarkovNode g) g

graphTick g 0 = g
graphTick g n = graphTick (updateGraph g) (n - 1)

main = do
    count <- (return . read . head) =<< getArgs
    let g = graphTick testGraph count
    putStrLn $ graphviz g "test_graph" (8.5,11.0) (1,1) Portrait
