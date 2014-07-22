import Data.List
import Data.String.Utils as S
import Data.Char
import System.Environment
import System.IO
import System.Random
import System.Random.Shuffle
import PrologLexer(lexString)
import Token
import Math.KMeans
import KMeansFork as K
import qualified Data.IntSet as I
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as B
import Graph
import Data.Graph.Inductive
import Data.Graph.Inductive.Example
import Data.Graph.Inductive.PatriciaTree


main = do
    file  <- getArgs
    contents <- readFile $ head file
    outh <- openFile ((!! 1) file) WriteMode
    let k = stringtoNumber $ (!! 2) file
    let y = map init $ map escapeUnusedVariables $ map followedBy$ (drop 2) $ splitSep $ lexString $ unlines $ deleteUnusedTerms $ lines contents -- gives just index 1- end
    print $ B.zip (B.fromList[0..(length (head y))])  (B.fromList $ head y)
    let numberString = (!!3) file
    g <- getStdGen
    let bin = findBin (head y)

    let x = splitSep $ lexString $ unlines $ escapeButRoot $ escapeButTransition $ lines contents
    let gr = delNode 0 $ buildGraph x empty
    let listOfNodes = nodes gr
    let distances =  fmap V.fromList $ calculateAllDistances ((length listOfNodes)-1) gr listOfNodes
    let work = map (addToTuple bin) $ transformList $  pickIndicesOfList y $ map stringtoNumber $ S.split "," numberString
    let test = addDistanceToTuple work distances
    print test
    --print work
    --print distances
    --print out
    --let output = generateDotOutput $ zip [1..] $ K.kmeans k K.euclidD K.iterativeSplit work
    --writeOutput outh output
    --hClose outh
    --print $ work



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
tokenToDouble (Bin i) = fromIntegral i
tokenToDouble (B False) = 0.0
tokenToDouble (B True) = 1.0
tokenToDouble (FDInt s) = read i :: Double where
    i = escapeFd s
tokenToDouble _ = error "tokenToDouble used on non-Int"


escapeFd :: String -> String
escapeFd [] = []
escapeFd (x:xs)
    | isDigit x
        = x : escapeFd xs
    | otherwise = escapeFd xs



indexElements :: [V.Vector Double] -> [(V.Vector Double,Int)]
indexElements xs = zip xs [1..]


transformList = indexElements . fmap V.fromList . (map (map tokenToDouble))




tokenToVector xs = V.fromList $ map tokenToDouble xs

stringtoNumber :: String -> Int
stringtoNumber x = read x :: Int


ioToString :: String -> [Char]
ioToString x = read x :: [Char]


generateDotOutput [] = ""
generateDotOutput (h:input) = "subgraph \"cluster_" ++ (show $ fst h) ++ "\" {node [style=filled, color = white]; label = \"CLUSTER"++(show $fst h)++ "\"; style = filled;color=" ++ (switchColor h)++"; "++ (replace "[" "" $ replace "]" "" $ replace "," ";" $ show $ extractIds $ snd h) ++ "}\n" ++ generateDotOutput input

generatePrologOutput :: (Show a1, Show b) => [(a1, [(a, b)])] -> [Char]
generatePrologOutput [] = ""
generatePrologOutput (h:input) = "cluster(" ++ (show $ fst h) ++ ", " ++ (show $ extractIds $ snd $ h) ++").\n"++ generatePrologOutput input

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

differenceBetweenBitVector xs ys = length $ filter id $ zipWith (/=) xs ys


addToTuple :: [c] -> (a,b) -> (a,(b,[c]))  --b may also be a Tuple
addToTuple ys (a,b) = (a,(b,ys))


addDistanceToTuple :: [(V.Vector Double, (Int, [Int]))] -> [V.Vector Int] -> [(V.Vector Double, (Int, [Int], V.Vector Int))]
addDistanceToTuple [] [] = []
addDistanceToTuple ((v,(i,l)):xs) (d:distance) = [(v,(i,l,d))] ++ addDistanceToTuple xs distance

