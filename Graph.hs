module Graph where


import PrologLexer(lexString)
import Data.List
import Data.List.Utils
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



-- code by http://stackoverflow.com/questions/16108714/haskell-removing-duplicates-from-a-list
removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

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
tokentoInt _ = error "tokenToInt used on non Int"

tokenToString :: Token -> String
tokenToString (Atom i) = i
tokenTostring _ = error "tokenToString used on non Atom"


replaceEdges :: [(Int, Int, String)] -> [(Int,[Int])] -> [(Int,Int, String)]
replaceEdges xs ys = replaceEdge xs (H.fromList $ setUpHashMap ys)

replaceEdge (a,b,t) hmap = (a',b',t) where
                           a' = lookup a hmap
                           b' = lookup b hmap


setUpHashMap [] = []
setUpHashMap (x:xs) = zip list (replicate (length list) (fst x) ) ++ setUpHashMap xs where list = (snd x)



vars:: [Integer]
vars = [1,2,3,4,5]