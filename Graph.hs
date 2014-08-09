module Graph where


import PrologLexer(lexString)
import Data.List
import Data.List.Utils
import Data.Maybe
import System.Environment
import Token
import Data.Graph.Inductive
import Data.Graph.Inductive.Example
import Data.Graph.Inductive.PatriciaTree
import qualified Data.HashMap as H


{-}
main = do
    file <- getArgs
    contents <- readFile $ head file
    let y = splitSep $ lexString $ unlines $ escapeButRoot $ escapeButTransition $ lines contents
    let gr = delNode 0 $ buildGraph y empty
    let xs = nodes gr
    print xs
    --print $ calculateAllDistances ((length xs) -1) gr xs
-}

buildGraph :: [[Token]] -> Gr () String -> Gr () String
buildGraph xs gr = insEdges (getAllEdges xs) $ insNodes (getAllNodes xs) gr


getAllEdges' ::[[Token]] -> [(Node, Node,())]
getAllEdges' xs = map toupleToEdge $ map toTouple $ map (map tokenToInt) xs
    where toTouple xxs = (head xxs, last xxs)
          toupleToEdge :: (Int,Int) -> (Node ,Node ,())
          toupleToEdge (x,y) = (x,y,())

getAllEdges :: [[Token]] -> [(Node, Node, String)]
getAllEdges xs = map toTripleN xs

toTriple :: [Token] -> (Int,Int,String)
toTriple xxs = (tokenToInt $ head xxs, tokenToInt $ last xxs, (tokenToString $ head $ tail xxs))

toTripleN :: [Token] -> (Node,Node,String)
toTripleN xxs = (tokenToInt $ head xxs, tokenToInt $ last xxs, (tokenToString $ head $ tail xxs))

getAllNodes :: [[Token]] -> [LNode ()]
getAllNodes xs = map intToNode $ map tokenToInt $ map head xs ++ map last xs


intToNode :: Int -> LNode ()
intToNode i = (i,())



escapeButRoot [] = []
escapeButRoot (x:xs) = if isPrefixOf "transition(root" x then escapeButTransition xs else [x] ++ escapeButTransition xs

escapeButTransition [] = []
escapeButTransition (x:xs)  = if isPrefixOf "transition(" x then [x] ++ escapeButTransition xs else escapeButTransition xs


extractEdges :: [[a]] -> [[a]]
extractEdges [] = []
extractEdges (x:xs) = [(head x) : [(last x)]] ++ extractEdges xs

calculateAllDistances :: Int -> Gr () () -> [Node] -> [[Int]]
calculateAllDistances 0 gr xs = [calcOneDistanceForAll 0 ((length xs)-1) gr xs]
calculateAllDistances n gr xs = [calcOneDistanceForAll n ((length xs)-1) gr xs] ++ calculateAllDistances (n-1) gr xs


calcOneDistanceForAll :: Int -> Int -> Gr () () -> [Node] -> [Int]
calcOneDistanceForAll n 0 gr xs = [calculateOneDistance n 0 gr xs]
--calcOneDistanceForAll n 0 gr xs = [[calculateOneDistance n 0 gr xs] ++ calcOneDistanceForAll (n-1) ((length xs)-1) gr xs]
calcOneDistanceForAll n m gr xs = [calculateOneDistance n m gr xs] ++ calcOneDistanceForAll n (m-1) gr xs






calculateOneDistance :: Int -> Int -> Gr () () -> [Node]-> Int
calculateOneDistance n m gr xs = (length $ esp ((!! n) xs) ((!! m) xs) gr)-1



splitSep xs
    | xs == [] = []
    | otherwise = let x = takeWhile (/= Sep '|') xs in
    x : splitSep (drop 1 $ dropWhile (/= Sep '|') xs)


tokenToInt:: Token -> Int
tokenToInt (Int i) = i
tokenToInt (Atom i) = 0
tokenToInt (Bin i) = i
tokenToInt (N i) = i
tokenToInt (S i) = 0
tokentoInt _ = error "tokenToInt used on non Int"

tokenToString :: Token -> String
tokenToString (Atom i) = i
tokenToString (S i) = i
tokenToString _ = []


replaceEdges :: [(Int, Int, String)] -> [(Int,[Int])] -> [(Int,Int, String)]
replaceEdges xs ys = map (replaceEdge (H.fromList $ setUpHashMap ys)) xs

replaceEdge :: H.Map Int Int -> (Int, Int, String) -> (Int, Int, String)
replaceEdge hmap (a,b,t) = (a',b',t) where
                           a' = zeroAsDefault $ H.lookup a hmap
                           b' = zeroAsDefault $ H.lookup b hmap

zeroAsDefault :: Maybe Int -> Int
zeroAsDefault mx = case mx of
    Nothing -> 0
    Just x -> x


setUpHashMap [] = []
setUpHashMap (x:xs) = zip list (replicate (length list) (fst x) ) ++ setUpHashMap xs where list = (snd x)
