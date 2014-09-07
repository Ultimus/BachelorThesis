import Graph
import Variables
import System.Environment
import System.IO
import System.Random
import System.Random.Shuffle
import qualified Data.Vector.Unboxed as V
import Data.String.Utils as S
import KMeansFork as K
import Data.List
import PrologLexer(lexString)


main = do
    print $ zip (init $ head vars) [0..] -- it is not possible to use the stateindex as a kmeans variable
    file  <- getArgs
    contents <- readFile $ head file
    outh <- openFile ((!! 1) file) WriteMode
    let k = read ((!! 2) file) :: Int
    let y = map init $ init vars
    let numberString = (!!3) file
    let c = read ((!! 4 ) file):: Int
    let distanceFunction = chooseDistanceFunction c
    let s = read ((!! 5) file) ::Int
    let splitFunction = chooseSplit s
    g <- getStdGen
    let longest = length $ decToBin $ tokenToInt $ (maximum (map maximum y))
    let work = indexElements (fmap V.fromList $ (map concat $ map (map (transformVariable longest)) $ pickIndicesOfList y $ map stringtoNumber $ S.split "," numberString)) (tokenToInt $ last $ head vars)
    let shfl = read ((!! 6)file ) ::Int
    let shuffled = shuffle' work (length work) g
    let concrete = if shfl == 1 then shuffled else work --using lazy evaluation
    let output = zip [1..] $ K.kmeans k distanceFunction splitFunction concrete
    if (length output) < k then print "Maximum Cluster size not reached" else print ""
    let clustersize = zip [1..] (map length (map snd output))
    print "Clustersize:"
    print clustersize

    let clAna = clusterAnalysis output
    print "Cluster Analysis : "
    print clAna
    let indices = zip [1.. k] $ map extractIds $ map snd output
    --Graph stuff

    let x = nubBy matchTuple $ compressLabels $ nub $ replaceEdges (map toTriple $ splitSep $ lexString $ unlines $ escapeButRoot $ escapeButTransition $ lines contents) indices

    writeOutput outh (generateCompressedDotOutput x clustersize clAna output)
    hClose outh
    print "finished"

    --let gr = delNode 0 $ buildGraph x empty
    --let listOfNodes = nodes gr
    --let distances =  fmap V.fromList $ calculateAllDistances ((length listOfNodes)-1) gr listOfNodes
    -- $ init $ averageDistanceByCluster indices distances
    --print  $ findMinMax (zip (fmap V.fromList $ map(map(tokenToDouble)) (findspecificvariables [1075,325,2778,1347,2508,112,1203,1953,455,1245,1091,1129,1637,845,1200,1207,1020,1211,1202,818,2823,690,2097,896,2818,1199,1917,1364,1201,2213,1206,868,1117,1149,1204,1378,909,560,363,1209,599,2010,2317,2367,658,1210,1426,810,1208,1451,2223,1388,1171,152,649,1399,1219,2285,127,1205,577,715,1106,2240,1969,721,1218,1860] vars)) [0..]) 0


    --let test = addDistanceToTuple work distances
