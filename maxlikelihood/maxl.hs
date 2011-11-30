import System (getArgs)
import Data.List

data Transition = Transition { tranStart :: Char, transEnd :: [Char] } deriving (Show)

instance Eq Transition where
    (==) a b = (tranStart a == tranStart b)
    (/=) a b = not (a == b)

addEnd t e = Transition (tranStart t) (e : (transEnd t))

updateTl oldList start end = let tmpList    = filter (\x -> start /= tranStart x) oldList
                                 prevTrans' = filter (\x -> start == tranStart x) oldList
                                 prevTrans  = if [] == prevTrans' then Transition start [] else head prevTrans' in
                             (addEnd prevTrans end) : tmpList

stats trans = let numOut = length (transEnd trans)
                  outs   = (group.sort) (transEnd trans) in
              concatMap (\x -> "P(" ++ [(tranStart trans)] ++ " -> " ++ [head x] ++ ") = " ++ (show ((fromIntegral $ length x) / (fromIntegral numOut)))++"\n") outs

parseList tl (x:[]) = tl
parseList tl (x:y:[]) = updateTl tl x y
parseList tl (x:y:zs) = let ntl = updateTl tl x y in
                        parseList ntl (y:zs)

main = do
    s <- getArgs
    let tl = foldl parseList [] s
    mapM_ (putStrLn . stats) tl
