module Graph where

import PrologLexer(lexString)
import Data.List
import Data.List.Utils
import Data.Char
import Data.Maybe
import System.Environment
import System.IO
import Token
import Data.Graph.Inductive
import Data.Graph.Inductive.Example
import Data.Graph.Inductive.PatriciaTree
import qualified Data.HashMap as H
import KMeansFork as K
import qualified Data.IntSet as I
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as B


toTriple :: [Token] -> (Int,Int,String)
toTriple xxs = (tokenToInt $ head xxs, tokenToInt $ last xxs, (tokenToString $ head $ tail xxs))

escapeButRoot [] = []
escapeButRoot (x:xs) = if isPrefixOf "transition(root" x then escapeButRoot xs else [x] ++ escapeButRoot xs

escapeButTransition [] = []
escapeButTransition (x:xs)  = if isPrefixOf "transition(" x then [x] ++ escapeButTransition xs else escapeButTransition xs

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

writeOutput outh outputString= do
    hPutStrLn outh outputString

--delete all unused Prolog terms that are not packed_visited_expressions
deleteUnusedTerms [] = []
deleteUnusedTerms (x:xs)  = if isPrefixOf "packed_visited_expression" x then [x] ++ deleteUnusedTerms xs else deleteUnusedTerms xs

pickIndicesOfList [] _ = []
pickIndicesOfList (x:xs) ys= [pickIndicesOfList' x ys] ++ pickIndicesOfList xs ys

pickIndicesOfList' _ [] = []
pickIndicesOfList' xs (y:ys) = [(!! y) xs] ++ pickIndicesOfList' xs ys


tokenToDouble :: Token -> Double
tokenToDouble (Int i) = fromIntegral i
tokenToDouble (Bin i) = fromIntegral i -- fromIntegral $ kardinality $ decToBin i
tokenToDouble (N i)= fromIntegral i
tokenToDouble (B False) = 0.0
tokenToDouble (B True) = 1.0
tokenToDouble (S i) = 0.0
tokenToDouble (FDInt s) = read i :: Double where
    i = escapeFd s
tokenToDouble _ = error "tokenToDouble used on non-Int"

intToBin :: Token -> Token
intToBin (Int x) = (Bin x)
intToBin _  = error "Int can not be converted to Bin"


escapeFd :: String -> String
escapeFd [] = []
escapeFd (x:xs)
    | isDigit x
        = x : escapeFd xs
    | otherwise = escapeFd xs

indexElements :: [V.Vector Double] -> Int -> [(V.Vector Double,Int)]
indexElements xs first = zip xs [first..]

transformList = indexElements . fmap V.fromList . (map (map tokenToDouble))

transformVariable _ (N i) = [tokenToDouble (N i)]
transformVariable _ (B True) = [tokenToDouble (B True)]
transformVariable _ (B False) = [tokenToDouble ( B False)]
transformVariable _ (Int i) = [tokenToDouble (Int i)]
transformVariable longest (Bin i) =  map fromIntegral $ fillingUpWithZero (decToBin i) longest
transformVariable longest (S i) = map fromIntegral $ replicate longest 0

stringtoNumber :: String -> Int
stringtoNumber x = read x :: Int

generateCompressedDotOutput [] _ _ _= ""
generateCompressedDotOutput xs cl clAna o= "digraph visited_states {\ngraph [nodesep=1.5, ranksep=1.5];\n/*\nClustersize: \n" ++ (show cl) ++ "\n ClusterAnalysis: \n" ++ (show clAna) ++ "\n ClusterIds:\n" ++ (generatePrologOutput o) ++"\n*/\n\n" ++ dotNodes xs ++"\n\n" ++ dotEdges xs ++ "\n}"

generatePrologOutput :: (Eq a1, Num a1, Show a1, Show b) => [(a1, [(a, b)])] -> [Char]
generatePrologOutput [] = ""
generatePrologOutput (h:input) = "cluster(" ++ (show $ fst h) ++ ", " ++ (show $ extractIds $ snd $ h) ++").\n"++ generatePrologOutput input


-- 1 -> 0 [color = "#006391", label="leave1", fontsize=12];

dotNodes :: [(Int,Int, String)] -> String
dotNodes xs = dotNodes' (H.toList $H.map nub $ H.map words (nodeLabel xs H.empty))


dotNodes' :: [(Int, [String])] -> String
dotNodes' [] = []
dotNodes' ((identifier, text):xs) = (show identifier) ++ " [shape = record, color = \"blue\", fontsize = 12, label = \"|{"++(show identifier) ++ " " ++ (mapShow text) ++"}|\"];\n" ++ dotNodes' xs

-- " [shape=record, color="blue", fontsize=12, label="|{new, swap\n|# states: 9}|"];

nodeLabel :: [(Int, Int, String)] -> H.Map Int String -> H.Map Int String
nodeLabel [] hmap= hmap
nodeLabel ((a,b,t):xs) hmap | H.member a hmap = nodeLabel xs hmap'
                            | otherwise  = nodeLabel xs (H.insert a t hmap)
                            where hmap' = H.insert a (t++" "++t') hmap
                                  t' = fromJust $ H.lookup a hmap


mapShow [] = []
mapShow (x:xs) =  x ++" "++ mapShow xs

dotEdges [] = []
dotEdges ((a,b,t):xs) = (show a) ++ " -> " ++ (show b) ++ " [color = \"#006391\", label=" ++ (show t) ++", fontsize=12];\n\n" ++ dotEdges xs

compressLabels :: [(Int, Int, String)]-> [(Int, Int, String)]
compressLabels [] = []
compressLabels ((a,b,t):xs) = [(a,b,t')] ++ compressLabels xs where t' = similarEdges (a,b,t) xs

similarEdges (a,b,t) [] = t
similarEdges (a,b,t) ((a', b' ,t'):xs) = if a == a' && b == b' then similarEdges (a,b, (t++" "++t')) xs else similarEdges (a,b,t) xs

matchTuple (a,b,t) (a',b',t') = if (a== 0 || b == 0 || a' == 0 || b' == 0) then True else a == a' && b == b'

switchColor :: (Eq a, Num a) => (a, b) -> [Char]
switchColor h
    | fst h == 1 = "lightgrey"
    | fst h == 2 = "lightblue"
    |fst h == 3 = "cornsilk"
    |fst h == 4 = "darkolivegreen1"
    |fst h == 5 = "coral1"
    |fst h == 6 = "aquamarine2"
    |fst h == 7 = "linen"
    |fst h == 8 = "thistle1"
    |fst h == 9 = "ivory3"
    |fst h == 10 = "snow2"

extractIds :: [(a,b)] -> [b]
extractIds = map snd

escapeUnusedVariables :: [Token] -> [Token]
escapeUnusedVariables xs = concatMap (\tok -> case tok of {S _ -> []; Atom _ -> []; _ -> [tok]}) xs

--http://stackoverflow.com/questions/1959715/how-to-print-integer-literals-in-binary-or-hex-in-haskell
decToBin x = reverse $ decToBin' x where
decToBin' 0 = []
decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a

chooseDistanceFunction :: Int -> Distance
chooseDistanceFunction x | x == 0 = K.euclidD
                         | x == 1 = K.l1Dist
                         | x == 2 = K.lInfDist

chooseSplit x | x == 0 = K.iterativeSplit
              | x == 1 = K.oneBigList

getAllIdsByCluster :: [(Int, [(V.Vector Double, Int)])]-> [[Int]]
getAllIdsByCluster [] = []
getAllIdsByCluster (x:xs) = [extractIds $ snd x] ++ getAllIdsByCluster xs

findMaximumLength:: [[Token]]-> Int -> Int
findMaximumLength xs index =  length $ decToBin $ maximum $ map tokenToInt $ map (!! index) xs

fillingUpWithZero :: [Int] -> Int -> [Int]
fillingUpWithZero xs 0 = xs
fillingUpWithZero xs i = if (i - (length xs)) == 0 then xs else fillingUpWithZero ([0]++ xs) i

setToBitVector :: [Int] ->[Int] -> [Int]  --second List is replicate (maximum) 0
setToBitVector [] ys = ys
setToBitVector (x:xs) ys = setToBitVector xs ((fst list) ++ [1] ++ (tail $ snd list))
                           where list = splitAt (x) ys

clusterAnalysis :: [(Int,[(V.Vector Double, Int)])]-> [(Int, [(Double, Double)])]
clusterAnalysis xs = map clusterAnalysis' xs

clusterAnalysis' :: (Int, [(V.Vector Double,Int)]) -> (Int, [(Double, Double)])
clusterAnalysis' x = ((fst x) , findMinMax (snd x) 0)

findMinMax :: [(V.Vector Double,Int)] -> Int->  [(Double, Double)]
findMinMax [] _= []
findMinMax vs i= if (i == (V.length (fst $ head vs))) then [] else [((minimum x), (maximum x))] ++ (findMinMax vs (i+1)) where x = map (V.! i) (map fst vs)

findspecificvariables [] _ = []
findspecificvariables (x:xs) y= [(!! x) y] ++ findspecificvariables xs y





--buildGraph :: [[Token]] -> Gr () String -> Gr () String
--buildGraph xs gr = insEdges (getAllEdges xs) $ insNodes (getAllNodes xs) gr
{-
containsBin:: [Token] -> Bool
containsBin [] = False
containsBin (Bin i : xs) = Bin i == Bin i
containsBin (_:xs) = containsBin xs

countBin :: Int -> [Token] -> Int
countBin x [] = x
countBin x (Bin i :xs) = countBin (x+1) xs
countBin x (_:xs) = countBin x xs

kardinality :: [Int] -> Int
kardinality = length . filter (==1)


averageDistanceByCluster :: [[Int]] -> [V.Vector Int] -> [Double]
averageDistanceByCluster [] _ = [0]
averageDistanceByCluster (i:indices) vectors = [(averageDistanceAmongCluster i vectors) / (fromIntegral $ length i)] ++ averageDistanceByCluster indices vectors

averageDistanceAmongCluster :: [Int] -> [V.Vector Int] -> Double
averageDistanceAmongCluster [] _ = 0
averageDistanceAmongCluster (i:indices) vector = (s / l) + averageDistanceAmongCluster indices vector
                                                 where distances = distanceToAllOtherNodes i indices vector
                                                       l = fromIntegral $ length distances
                                                       s = fromIntegral $ sum distances

distanceToAllOtherNodes::Int -> [Int] -> [V.Vector Int] -> [Int]
distanceToAllOtherNodes _ [] _ = [0]
distanceToAllOtherNodes x (i:indices) vector =  [(V.! (i-1)) $ (!! (x-1)) vector] ++ distanceToAllOtherNodes x indices vector





addToTuple :: [c] -> (a,b) -> (a,(b,[c]))  --b may also be a Tuple
addToTuple ys (a,b) = (a,(b,ys))


addDistanceToTuple :: [(V.Vector Double, (Int, [Int]))] -> [V.Vector Int] -> [(V.Vector Double, (Int, [Int], V.Vector Int))]
addDistanceToTuple [] [] = []
addDistanceToTuple ((v,(i,l)):xs) (d:distance) = [(v,(i,l,d))] ++ addDistanceToTuple xs distance


findBin :: [Token] -> [Int]
findBin myTokenList = [ ix | (ix, Bin _) <- zip [0..] myTokenList ]

tokenToVector xs = V.fromList $ map tokenToDouble xs



followedBy:: [Token]-> [Token]
followedBy [] = []
followedBy [x] = [x]
followedBy (x:y:xs) = if x == Sep '-' then  [(intToBin y)] ++ followedBy xs else x : followedBy (y:xs)

deleteSimilarEdges' [] = []
deleteSimilarEdges' (x:xs) = deleteSimilarEdges x xs ++deleteSimilarEdges' xs

deleteSimilarEdges :: (Int, Int , String) -> [(Int, Int, String)] -> [(Int, Int, String)]
deleteSimilarEdges (a,b,t) [] = [(a,b,t)]
deleteSimilarEdges (a,b,t) ((a',b',t'):xs) = if a == a' && b == b' then deleteSimilarEdges (a,b,t) xs else [(a',b',t')] ++deleteSimilarEdges (a,b,t) xs


chooseOutput:: (Eq a1, Num a1, Show a1, Show b) => [Char] -> [(a1, [(a, b)])] -> [Char]
chooseOutput name = if isPrefixOf "lp." (reverse name) then generatePrologOutput else generateDotOutput

generateDotOutput :: (Eq a1, Num a1, Show a1, Show b) => [(a1, [(a, b)])] -> [Char]
generateDotOutput [] = ""
generateDotOutput (h:input) = "subgraph \"cluster_" ++ (show $ fst h) ++ "\" {node [style=filled, color = white]; label = \"CLUSTER"++(show $fst h)++ "\"; style = filled;color=" ++ (switchColor h)++"; "++ (replace "[" "" $ replace "]" "" $ replace "," ";" $ show $ extractIds $ snd h) ++ "}\n" ++ generateDotOutput input


getAllEdges' ::[[Token]] -> [(Node, Node,())]
getAllEdges' xs = map toupleToEdge $ map toTouple $ map (map tokenToInt) xs
    where toTouple xxs = (head xxs, last xxs)
          toupleToEdge :: (Int,Int) -> (Node ,Node ,())
          toupleToEdge (x,y) = (x,y,())

ioToString :: String -> [Char]
ioToString x = read x :: [Char]

toTripleN :: [Token] -> (Node,Node,String)
toTripleN xxs = (tokenToInt $ head xxs, tokenToInt $ last xxs, (tokenToString $ head $ tail xxs))

getAllNodes :: [[Token]] -> [LNode ()]
getAllNodes xs = map intToNode $ map tokenToInt $ map head xs ++ map last xs


intToNode :: Int -> LNode ()
intToNode i = (i,())

getAllEdges :: [[Token]] -> [(Node, Node, String)]
getAllEdges xs = map toTripleN xs

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

--pick k numbers random
--code from : http://codereview.stackexchange.com/questions/36671/function-to-produce-unique-random-numbers

uniqueRandomInts :: RandomGen g => (Int, Int) -> Int -> g -> ([Int], g)
uniqueRandomInts range@(min, max) n rng
    | n < 0             = error "n must be positive"
    | n > max - min + 1 = error "n is too large for range"
    | otherwise         = recursion n I.empty ([], rng)
    where
        recursion n used (xs, rng)
            | n == 0          = (xs, rng)
            | I.member x used = recursion n used (xs, rng')
            | otherwise       = recursion (n-1) (I.insert x used) (x:xs, rng')
            where
                (x, rng') = randomR range rng

--prints all indices of a vector
--prettyPrint xs = print $ zip [0..] xs

--constructListOfLists [x] = [[x]]
--constructListOfLists (x:xs) = [x]:constructListOfLists xs

--pick a specific index of all vectors in a new vector and start k means
--pickIndexOfVectors n 0 xs = B.fromList[(!! n) $ (B.! 0) xs]
--pickIndexOfVectors n m xs = B.fromList[(!! n) $ (B.! m) xs] B.++ pickIndexOfVectors n (m-1) xs

--pickIndexOfLists n 0 xs = [(!!n) $ (!! 0) xs]
--pickIndexOfLists n m xs = [(!! n ) $ (!! m) xs] ++ pickIndexOfLists n (m-1) xs

 -}

