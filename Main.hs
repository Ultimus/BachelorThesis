import Data.List
import Data.String.Utils as S
import Data.Char
import Data.Maybe
import System.Environment
import System.IO
import System.Random
import System.Random.Shuffle
import PrologLexer(lexString)
import Token
import KMeansFork as K
import qualified Data.IntSet as I
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as B
import Graph
import Data.Graph.Inductive
import Data.Graph.Inductive.Example
import Data.Graph.Inductive.PatriciaTree
import Variables
import qualified Data.HashMap as H


main = do
    file  <- getArgs
    contents <- readFile $ head file
    outh <- openFile ((!! 1) file) WriteMode
    let k = read ((!! 2) file) :: Int
    let y = map init $ init vars -- escape empty list at the end --  map init $ map escapeUnusedVariables $ map followedBy$ (drop 2) $ splitSep $ lexString $ unlines $ deleteUnusedTerms $ lines contents -- gives just index 1- end
    --print $ B.zip (B.fromList[0..(length (head y))])  (B.fromList $ head y)
    print $ zip (init $ head vars) [0..] -- it is not possible to use the stateindex as a kmeans variable
    let numberString = (!!3) file
    let c = read ((!! 4 ) file):: Int
    let distanceFunction = chooseDistanceFunction c
    let s = read ((!! 5) file) ::Int
    let splitFunction = chooseSplit s
    g <- getStdGen
    let longest =  (maximum $ map (countBin 0) y)+1
    let work = indexElements (fmap V.fromList $ (map concat $ map (map (transformVariable longest)) $ pickIndicesOfList y $ map stringtoNumber $ S.split "," numberString)) (tokenToInt $ last $ head vars)
    --let outputFileType = chooseOutput ((!! 1) file)
    let shfl = read ((!! 6)file ) ::Int
    let shuffled = shuffle' work (length work) g
    let concrete = if shfl == 1 then shuffled else work --using lazy evaluation
    let output = zip [1..] $ K.kmeans k distanceFunction splitFunction concrete
    let indices = zip [1.. k] $ map extractIds $ map snd output
    --Graph stuff

    let x = nubBy matchTuple $ compressLabels $ nub $ replaceEdges (map toTriple $ drop 1 $ splitSep $ lexString $ unlines $ escapeButRoot $ escapeButTransition $ lines contents) indices
    --let gr = delNode 0 $ buildGraph x empty
    --let listOfNodes = nodes gr
    --let distances =  fmap V.fromList $ calculateAllDistances ((length listOfNodes)-1) gr listOfNodes
    writeOutput outh (generateCompressedDotOutput x)
    hClose outh
   -- print $ findMaximumLength (init vars) 1
    print x
    -- $ init $ averageDistanceByCluster indices distances


    --let test = addDistanceToTuple work distances

writeOutput outh outputString= do
    hPutStrLn outh outputString

--delete all unused Prolog terms that are not packed_visited_expressions
deleteUnusedTerms [] = []
deleteUnusedTerms (x:xs)  = if isPrefixOf "packed_visited_expression" x then [x] ++ deleteUnusedTerms xs else deleteUnusedTerms xs


--lexString gives a list of tokens. SplitSep shall split the list at each
-- Sep '|' and produce a list of lists which can be used for concrete calculating
{-splitSep xs
    | xs == [] = []
    | otherwise = let x = takeWhile (/= Sep '|') xs in
    x : splitSep (drop 1 $ dropWhile (/= Sep '|') xs)
-}
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
prettyPrint xs = print $ zip [0..] xs

--pick a specific index of all vectors in a new vector and start k means
pickIndexOfVectors n 0 xs = B.fromList[(!! n) $ (B.! 0) xs]
pickIndexOfVectors n m xs = B.fromList[(!! n) $ (B.! m) xs] B.++ pickIndexOfVectors n (m-1) xs

pickIndexOfLists n 0 xs = [(!!n) $ (!! 0) xs]
pickIndexOfLists n m xs = [(!! n ) $ (!! m) xs] ++ pickIndexOfLists n (m-1) xs

pickIndicesOfList [] _ = []
pickIndicesOfList (x:xs) ys= [pickIndicesOfList' x ys] ++ pickIndicesOfList xs ys

pickIndicesOfList' _ [] = []
pickIndicesOfList' xs (y:ys) = [(!! y) xs] ++ pickIndicesOfList' xs ys


constructListOfLists [x] = [[x]]
constructListOfLists (x:xs) = [x]:constructListOfLists xs



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






tokenToVector xs = V.fromList $ map tokenToDouble xs

stringtoNumber :: String -> Int
stringtoNumber x = read x :: Int


ioToString :: String -> [Char]
ioToString x = read x :: [Char]

chooseOutput:: (Eq a1, Num a1, Show a1, Show b) => [Char] -> [(a1, [(a, b)])] -> [Char]
chooseOutput name = if isPrefixOf "lp." (reverse name) then generatePrologOutput else generateDotOutput

generateDotOutput :: (Eq a1, Num a1, Show a1, Show b) => [(a1, [(a, b)])] -> [Char]
generateDotOutput [] = ""
generateDotOutput (h:input) = "subgraph \"cluster_" ++ (show $ fst h) ++ "\" {node [style=filled, color = white]; label = \"CLUSTER"++(show $fst h)++ "\"; style = filled;color=" ++ (switchColor h)++"; "++ (replace "[" "" $ replace "]" "" $ replace "," ";" $ show $ extractIds $ snd h) ++ "}\n" ++ generateDotOutput input

generatePrologOutput :: (Eq a1, Num a1, Show a1, Show b) => [(a1, [(a, b)])] -> [Char]
generatePrologOutput [] = ""
generatePrologOutput (h:input) = "cluster(" ++ (show $ fst h) ++ ", " ++ (show $ extractIds $ snd $ h) ++").\n"++ generatePrologOutput input

generateCompressedDotOutput [] = ""
generateCompressedDotOutput xs = "digraph visited_states {\ngraph [nodesep=1.5, ranksep=1.5];\n" ++ dotNodes xs ++"\n\n" ++ dotEdges xs ++ "\n}"

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


{-}
deleteSimilarEdges' [] = []
deleteSimilarEdges' (x:xs) = deleteSimilarEdges x xs ++deleteSimilarEdges' xs

deleteSimilarEdges :: (Int, Int , String) -> [(Int, Int, String)] -> [(Int, Int, String)]
deleteSimilarEdges (a,b,t) [] = [(a,b,t)]
deleteSimilarEdges (a,b,t) ((a',b',t'):xs) = if a == a' && b == b' then deleteSimilarEdges (a,b,t) xs else [(a',b',t')] ++deleteSimilarEdges (a,b,t) xs
-}

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



followedBy:: [Token]-> [Token]
followedBy [] = []
followedBy [x] = [x]
followedBy (x:y:xs) = if x == Sep '-' then  [(intToBin y)] ++ followedBy xs else x : followedBy (y:xs)

intToBin :: Token -> Token
intToBin (Int x) = (Bin x)
intToBin _  = error "Int can not be converted to Bin"


findBin :: [Token] -> [Int]
findBin myTokenList = [ ix | (ix, Bin _) <- zip [0..] myTokenList ]


--http://stackoverflow.com/questions/1959715/how-to-print-integer-literals-in-binary-or-hex-in-haskell
decToBin x = reverse $ decToBin' x where
decToBin' 0 = []
decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a





addToTuple :: [c] -> (a,b) -> (a,(b,[c]))  --b may also be a Tuple
addToTuple ys (a,b) = (a,(b,ys))


addDistanceToTuple :: [(V.Vector Double, (Int, [Int]))] -> [V.Vector Int] -> [(V.Vector Double, (Int, [Int], V.Vector Int))]
addDistanceToTuple [] [] = []
addDistanceToTuple ((v,(i,l)):xs) (d:distance) = [(v,(i,l,d))] ++ addDistanceToTuple xs distance

chooseDistanceFunction :: Int -> Distance
chooseDistanceFunction x | x == 0 = K.euclidD
                         | x == 1 = K.l1Dist
                         | x == 2 = K.lInfDist

chooseSplit x | x == 0 = K.iterativeSplit
              | x == 1 = K.oneBigList


getAllIdsByCluster :: [(Int, [(V.Vector Double, Int)])]-> [[Int]]
getAllIdsByCluster [] = []
getAllIdsByCluster (x:xs) = [extractIds $ snd x] ++ getAllIdsByCluster xs


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


kardinality :: [Int] -> Int
kardinality = length . filter (==1)


findMaximumLength:: [[Token]]-> Int -> Int
findMaximumLength xs index =  length $ decToBin $ maximum $ map tokenToInt $ map (!! index) xs

fillingUpWithZero :: [Int] -> Int -> [Int]
fillingUpWithZero xs 0 = xs
fillingUpWithZero xs i = if (i - (length xs)) == 0 then xs else fillingUpWithZero ([0]++ xs) i


setToBitVector :: [Int] ->[Int] -> [Int]  --second List is replicate (maximum) 0
setToBitVector [] ys = ys
setToBitVector (x:xs) ys = setToBitVector xs ((fst list) ++ [1] ++ (tail $ snd list))
                           where list = splitAt (x) ys

containsBin:: [Token] -> Bool
containsBin [] = False
containsBin (Bin i : xs) = Bin i == Bin i
containsBin (_:xs) = containsBin xs

countBin :: Int -> [Token] -> Int
countBin x [] = x
countBin x (Bin i :xs) = countBin (x+1) xs
countBin x (_:xs) = countBin x xs




