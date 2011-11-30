import Control.Monad

data Direction = NORTH | SOUTH | EAST | WEST deriving (Eq, Enum, Show)
data GridWorld = GridWorld { sizeX :: Int, sizeY :: Int, gridObstacles :: [(Int, Int)], gridValues :: [Float] } deriving (Show)
data GridActor = GridActor { positionX :: Int, positionY :: Int } deriving (Show, Eq)

directionCoordinates NORTH = ( 0 ,1)
directionCoordinates SOUTH = ( 0,-1)
directionCoordinates EAST  = ( 1,0 )
directionCoordinates WEST  = (-1,0 )

world = GridWorld 3 1 [] [-100,0,0,100,0,0,0,0]

actorCoords a = (positionX a, positionY a)
replaceAt l x e = take x l ++ [e] ++ drop (x+1) l

gridCoordElem world (x,y) = (y * (sizeX world + 1)) + x

replaceValue world c@(x,y) v = GridWorld (sizeX world) (sizeY world) (gridObstacles world) newValues where
    newValues = replaceAt (gridValues world) (gridCoordElem world c) v

(x,y) `tPlus` (x',y') = (x+x',y+y')

inBounds gridWorld (x,y) = (x >= 0) && (x <= sizeX gridWorld) && (y >= 0) && (y <= sizeY gridWorld)

gridGetValue world c = gridValues world !! gridCoordElem world c

actorGetValue world actor = gridGetValue world $ actorCoords actor

actorMove world actor direction =
    GridActor newX newY where
        (newX, newY) = let ac = actorCoords actor
                           dc = directionCoordinates direction
                           nc = tPlus ac dc in
            if inBounds world nc then (if any (==nc) (gridObstacles world) then ac else nc) else ac

moveProb world actor NORTH = [(0.8,actorMove world actor NORTH),
                              (0.2,actorMove world actor SOUTH)]

moveProb world actor WEST  = [(0.8,actorMove world actor WEST),
                              (0.2,actorMove world actor EAST)]

moveProb world actor SOUTH = [(0.8,actorMove world actor SOUTH),
                              (0.2,actorMove world actor NORTH)]

moveProb world actor EAST  = [(0.8,actorMove world actor EAST),
                              (0.2,actorMove world actor WEST)]

-- moveProb world actor NORTH = [(0.8,actorMove world actor NORTH)
--                              ,(0.1,actorMove world actor EAST)
--                              ,(0.1,actorMove world actor WEST)]
-- 
-- moveProb world actor SOUTH = [(0.8,actorMove world actor SOUTH)
--                              ,(0.1,actorMove world actor EAST)
--                              ,(0.1,actorMove world actor WEST)]
-- 
-- moveProb world actor EAST = [(0.8,actorMove world actor EAST)
--                             ,(0.1,actorMove world actor NORTH)
--                             ,(0.1,actorMove world actor SOUTH)]
-- 
-- moveProb world actor WEST = [(0.8,actorMove world actor WEST)
--                             ,(0.1,actorMove world actor NORTH)
--                             ,(0.1,actorMove world actor SOUTH)]

actProb w a d = sum $ map (\(x,y) -> x * actorGetValue w y) (moveProb w a d)

bestDirection world act =
    let nv = map (\x -> (x, (actorGetValue world . actorMove world act) x)) [NORTH,SOUTH,EAST,WEST] in
    head [fst x | x <- nv, snd x == (maximum . snd $ unzip nv)]

actorCalcValue world act g r = let d = bestDirection world act in
    g * actProb world act d + r

-- updateWorld world actor = replaceValue world (actorCoords actor) (actorCalcValue world actor 1.0 (-3))
updateWorld world actor = replaceValue world (actorCoords actor) (actorCalcValue world actor 1.0 (-4))

calcPath world (c:[]) = updateWorld world (uncurry GridActor c)
    
calcPath world (c:cl) = let w = calcPath world cl in
    updateWorld w (uncurry GridActor c)
