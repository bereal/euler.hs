import System.Environment
import System.Directory
import Data.List
import Data.Maybe
import Data.Function
import Data.Char
import Numeric
import Debug.Trace

-- Utils

infixl 6 ?
True ? x = const x
False ? _ = id

enum = zip [0..]

trace' v = trace (show v) v

sublists len arr =
    if length arr < len
        then []
        else (take len arr : (sublists len $ tail arr))

fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

factorial n = product [2..n]

splitOn :: (a->Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f ls =
    case dropWhile f ls of
        [] -> []
        ls' -> part : splitOn f rest
            where (part, rest) = break f ls'

isInteger = (==0).snd.properFraction

alphaScore =
    let alphaNum c = ord c - base
            where base = ord 'A' - 1

    in sum . (map alphaNum)

-- A primes sieve from haskell.org:

primes = 2 : eratos [3,5..]  where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p*p,p*p+2*p..])

primesTo n = 2 : eratos [3,5..n]  where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p*p,p*p+2*p..n])

isPrime n = n>1 && length (primeFactors n) == 1

minus (x:xs) (y:ys) = case (compare x y) of
    LT -> x : minus xs (y:ys)
    EQ -> minus xs ys
    GT -> minus (x:xs) ys
minus xs _ = xs

primeFactors n = factor n $ primesTo n
  where
    factor n (p:ps)
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      = factor n ps

cartesianLists :: [[a]] -> [[a]]
cartesianLists [] = []
cartesianLists [hs] = [[h] | h<-hs ]
cartesianLists (hs:ts) =
    let cart' = cartesianLists ts
    in [h:c | c<-cart', h<-hs]

allFactors :: Int -> [Int]
allFactors 1 = [1]
allFactors n =
    let pf = group $ primeFactors n
        prims = map head pf
        powers = map length  pf
        mkFactor pows = product $ zipWith (^) prims pows
    in
        init $ map mkFactor $ cartesianLists [[0..p] | p<-powers]


-- Problems ------------------------------------------------

data Problem = Problem { pId :: Int, dataString :: Maybe String }

euler :: Problem -> Int

-- Problem 1 -----------------------------------------------

euler Problem {pId = 1} =
    let divisors = [3,5]
        limit    = 1000
        isGood n = any (==0) $ map (mod n) divisors
        goodNums = filter isGood [1..limit-1]
    in sum goodNums


-- Problem 2 -----------------------------------------------

euler Problem {pId = 2} =
    let limit = 4000000
        nums = takeWhile (<=limit) fibonacci
    in sum $ filter even nums


-- Problem 3 -----------------------------------------------

euler Problem {pId = 3}  = maximum $ primeFactors 600851475143


-- Problem 4 -----------------------------------------------

euler Problem {pId = 4}  =
    let palindromes = map mkPalind [999,998..100]
            where mkPalind n = read $ s ++ (reverse s) where s = show n

        -- We can use the fact that any palindrome of 2*n digits is a multiple of 11
        divisors = [990, 979..100]
        isGoodDiv n d = let (q, r) = (n `divMod` d)
                        in (r==0 && q < 1000 && q > 100)
        isSolution n = isJust $ find (isGoodDiv n) divisors
    in head $ filter isSolution palindromes


-- Problem 5 -----------------------------------------------

euler Problem {pId = 5}  = foldr1 lcm [1..20]


-- Problem 6 -----------------------------------------------

euler Problem {pId = 6} =
    let nums = [1..100]
        sumSq = sum . map (^2)
        sqSum = (^2) . sum
    in (sqSum nums) - (sumSq nums)


-- Problem 7 -----------------------------------------------

euler Problem {pId = 7}  = head $ drop 10000 $ primes


-- Problem 8 -----------------------------------------------

euler Problem {pId = 8, dataString=Nothing} = error "Data file is missing"
euler Problem {pId = 8, dataString=Just sData} =
    let
        digits = [digitToInt d | d <- sData, isDigit d]
    in
        maximum [product arr | arr <- sublists 5 digits]


-- Problem 9 -----------------------------------------------

euler Problem {pId = 9} =
    let param = 1000
        isSolution (x, y) = x^2 + y^2 == (param - x - y)^2
        (a, b) = head $ filter isSolution [(x, y) | x <- [1..1000], y <- [1..1000-x]]
    in a * b * (param - a - b)


-- Problem 10 ----------------------------------------------

euler Problem {pId = 10} = sum $ primesTo 2000000


-- Problem 11 ----------------------------------------------

euler Problem {pId = 11, dataString = Just sData} =
    let
        rows = [map read $ words l | l <- (lines sData)]
        cols = transpose rows
        diags matrix = transpose [drop i r | (i,r) <- enum matrix]
        allDiags = [rows, cols, (map reverse rows), (map reverse cols)] >>= diags
        allLines = rows ++ cols ++ allDiags
        groups = concatMap (sublists 4) allLines
    in
        maximum $ map product groups


-- Problem 12 ----------------------------------------------

euler Problem {pId = 12} =
    let triangles = scanl1 (+) [1..]
        countDivisors n = product $ map ((+1).length) $ group $ primeFactors n
        isSolution n = countDivisors n > 500
    in head $ filter isSolution triangles


-- Problem 13 ----------------------------------------------

euler Problem {pId = 13, dataString=Nothing} = error "Data file is missing"
euler Problem {pId = 13, dataString = Just sData}
    = let nums = map read $ lines sData
          s = sum nums
          digitsStr = take 10 $ show s
    in read digitsStr


-- Problem 14 ----------------------------------------------

euler Problem {pId = 14}  =
    let num = 1000000
        next n
            | even n = n `div` 2
            | otherwise = 3 * n + 1
        seqLen' buf 1 = buf
        seqLen' buf n = seqLen' (buf+1) (next n)
        seqLen = seqLen' 0
    in maximumBy (compare `on` seqLen) [(num `div` 2)..num]


-- Problem 15 ----------------------------------------------

euler Problem {pId = 15} =
    let
        size = 20
        dsize = 2 * size
        combs n k = (product [n - k + 1..n]) `div` (factorial k)
        result = 2 * (sum[combs (dsize - m - 1) (size-1) | m <- [1..size]])
    in
        fromInteger result


-- Problem 16 ----------------------------------------------

euler Problem {pId = 16} = sum $ map digitToInt $ show $ 2^1000


-- Problem 17 ----------------------------------------------

euler Problem {pId = 17} =
    let
        below20 = ["", "one", "two", "three", "four", "five",
                   "six", "seven", "eight", "nine", "ten",
                   "eleven", "twelve", "thirteen", "fourteen", "fifteen",
                   "sixteen", "seventeen", "eighteen", "nineteen"]

        decs = ["", "", "twenty", "thirty", "forty", "fifty",
                "sixty", "seventy", "eighty", "ninety"]

        say n
            | n < 20 = below20 !! n
            | n < 100 =
                let (d, r) = n `divMod` 10
                    sayDecs = decs !! d
                    sayRest = "-" ++ (say r)
                in sayDecs ++ (if r==0 then "" else sayRest)
            | n < 1000 = let (d, r) = n `divMod` 100
                             sayHund = (say d) ++ " hundred"
                             sayRest = " and " ++ (say r)
                in sayHund ++ (if r==0 then "" else sayRest)
            | n == 1000 = "one thousand"
            | otherwise = undefined

    in
        length $ filter isAlpha ([1..1000] >>= say)


-- Problem 18 ----------------------------------------------

euler Problem {pId = 18, dataString = Nothing} = error "Tree data file is missing"
euler Problem {pId = 18, dataString = Just sData} =
    let tree = [map read $ words l | l <- lines sData]
        maxFromPairs (v1: v2: vs) = (max v1 v2) : maxFromPairs (v2:vs)
        calcPaths [] level = level
        calcPaths kidPaths level = zipWith (+) level (maxFromPairs kidPaths)
    in
        head $ foldl calcPaths [] $ reverse tree


-- Problem 19 ----------------------------------------------

euler Problem {pId = 19} =
    let isLeap year = year `mod` 4 == 0 -- enough for the XX century
        smod v m = 1 + ((v-1) `mod` m)
        months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        addWeek (y, m, d) =
            let
                monLen = months !! (m-1) + fromEnum (m==2 && (isLeap y))
                newD = d+7
                newM = m + fromEnum (newD > monLen)
                newY = y + fromEnum (newM > 12)
            in (newY, newM `smod` 12, newD `smod` monLen)

        sundays = iterate addWeek (1900, 1, 7)
        isDay d (_, _, d') = d == d'
        isXX (y, _, _) = y <= 2000 && y >= 1901
        findSub f l = takeWhile f $ dropWhile (not.f) l
    in
        length $ findSub isXX $ filter (isDay 1) sundays


-- Problem 20 ----------------------------------------------

euler Problem {pId = 20} = sum $ map digitToInt $ show $ factorial 100


-- Problem 21 ----------------------------------------------

euler Problem {pId = 21} =
    let isAmicable n =
            let factors = allFactors n
                candidate = sum factors
            in  n == (sum $ allFactors candidate) && n/=candidate
    in sum $ filter isAmicable [1..9999]


-- Problem 22 ----------------------------------------------

euler Problem {pId = 22, dataString=Just sData} =
    let w0rds = splitOn (not . isAlpha) sData
        sortedWords = sort w0rds
    in sum $ zipWith (*) [1..] (map alphaScore sortedWords)


-- Problem 23 ----------------------------------------------

euler Problem {pId = 23} =
    let
        limit = 28123
        isAbundant x = (sum $ allFactors x) > x
        abundants = filter isAbundant [1..limit]
        diffAbundant n a = isAbundant (n-a)
        isSum n = isJust $ find (diffAbundant n) $ takeWhile (< n) abundants
    in sum $ filter (not . isSum) [1..limit]


-- Problem 24 ----------------------------------------------

euler Problem {pId = 24} =
    let splitAtBrokenOrder (x: y: rest) buf
            | y < x = (buf ++ [x], y, rest)
            | otherwise = splitAtBrokenOrder (y: rest) (buf++[x])

        next s = -- deal with reversed list
            let (left, x, right) = splitAtBrokenOrder s []
                (left', (y : right')) =  span (<x) left
            in (reverse (left' ++ (x:right'))) ++ (y:right)

        start = ['0'..'9']

        iter f x n = iter' n x where
            iter' 1 buf = buf
            iter' n buf = iter' (n-1) $ f buf
    in
        read $ reverse $ iter next (reverse start) $ 1000000


-- Problem 25 ----------------------------------------------

euler Problem {pId = 25} =
    let f x y n = if (length $ show y) == 1000 then n else f y (y+x) (n+1)
    in f 0 1 1


-- Problem 26 ----------------------------------------------

euler Problem {pId = 26} =
    let divide rem val =
            case rem `divMod` val of
                (q, 0) -> [(rem, q)]
                (q, r) -> (rem, q) : (divide (r*10) val)

        findLoop sequence = findLoop' [] sequence
            where
                findLoop' buf [] = buf
                findLoop' buf (s:rest) = case (findIndex (==s) buf) of
                    Just i -> reverse $ take (i+1) buf
                    Nothing -> findLoop' (s:buf) rest

        loopLen = length . findLoop . (divide 10)

    in maximumBy (compare `on` loopLen) [1..1000]


-- Problem 27 ----------------------------------------------

euler Problem {pId = 27} =
    let getSequence a b = map (\n -> (n + a) * n + b) [1..]
        primeSeqLen (a, b) = length $ takeWhile isPrime (getSequence a b)
        cmp = compare `on` primeSeqLen
        (a, b) = maximumBy cmp [(a',b') | a'<-[-999,-997..1000],
                                          b'<-[-a',-a'+2..1000]]
    in
        (a*b)


-- Problem 28 ----------------------------------------------

euler Problem {pId = 28} =
    let calcCorners buf = buf ++ [[start+step*i | i<-[0..3]]]
            where size = 2*(length buf) + 1
                  step = size - 1
                  start = step + (last $ last buf)

        corners = iterate calcCorners [[1]]
        spiralSize = 1001
        spiralCorns = corners !! ((spiralSize-1) `div` 2)
    in
        sum $ map sum spiralCorns

-- Problem 29 ----------------------------------------------

euler Problem {pId = 29} =
    let vals = [a^b | a<-[2..100], b<-[2..100]]
    in length $ group $ sort $ vals


-- Problem 30 ----------------------------------------------

euler Problem {pId = 30} =
    let isGood n = n == (sum $ map ((^5).digitToInt) $ show n)
    in sum $ filter isGood [2..1000000]


-- Problem 31 ----------------------------------------------

euler Problem {pId = 31} =
    let coins = [1, 2, 5, 10, 20, 50, 100, 200]
        solve cs v
            | v == 0 = 1
            | cs == [] = 0
            | otherwise = let (c:cs') = cs
                in if c > v then 0
                else (solve cs $ v - c) + (solve cs' v)
    in solve coins 200

-- Problem 34 ----------------------------------------------

euler Problem {pId = 34} =
    let isGood n = n == (sum $ map (factorial.digitToInt) $ show n)
    in sum $ filter isGood [3..3000000]


-- Problem 35 ----------------------------------------------

euler Problem {pId = 35} =
    let rotate (x:xs) = xs ++ [x]
        rotations l = take (length l) $ iterate rotate l
        isCircularPrime = (all isPrime).(map read).tail.rotations.show
        -- ^^^ tail is because iterating through primes
        -- and don't need to check the original number

    in length $ filter isCircularPrime $ primesTo 1000000



-- Problem 36 ----------------------------------------------

euler Problem {pId = 36} =
    let isPalind s = (head rev /= '0') && (s == rev)
            where rev = reverse s
        isPalind2_10 n = (isPalind s10) && (isPalind s2)
            where s10 = showIntAtBase 2 intToDigit n ""
                  s2  = show n
        limit = 1000000
    in sum $ filter isPalind2_10 [1..(limit-1)]


-- Problem 40 ----------------------------------------------

euler Problem {pId = 40} =
    let digits = map digitToInt $ [1..] >>= show
    in product $ map (\n -> digits !! (10^n-1)) [0..6]


-- Problem 42 ----------------------------------------------

euler Problem {pId = 42, dataString=Nothing} = error "Data file is missing"
euler Problem {pId = 42, dataString = Just sData} =
    -- n^2 + n - 2a = 0 ; when does this equation have natural solutions?
    let discriminant a = 8*a + 1
        isTriangle = isInteger.sqrt.discriminant.fromIntegral
        scores = map alphaScore $ splitOn (not.isAlpha) sData
    in
        length $ filter isTriangle scores


-- Problem 45 ----------------------------------------------

euler Problem {pId = 45} =
    let triangles = scanl1 (+) [1..]
        hasIntRoots a b c = (isInteger sqd) && ((-b + (round sqd)) `mod` (2*a) ==0 )
            where sqd = sqrt $ fromIntegral $ (b^2 - 4*a*c)
        isPentagonal n = hasIntRoots 3 (-1) (-2*n)
        isHexagonal n = hasIntRoots 2 (-1) (-n)
        isGood a = isPentagonal a && isHexagonal a
    in head $ filter isGood $ dropWhile (<=40755) triangles


-- Problem 48 ----------------------------------------------

euler Problem {pId = 48} =
    let last10 = read . reverse . (take 10) . reverse . show
    in last10 $ foldl1 (+) (map (\x->x^x) [1..1000])


-- Problem 52 ----------------------------------------------

euler Problem {pId = 52} =
    let isGoodNum n =
            let (h:t) = map (sort.show.(n*)) [2..6]
            in all (== h) t

    in head $ filter isGoodNum [1..]

-- Problem 67 ----------------------------------------------

euler Problem {pId = 67, dataString = Nothing} = error "Tree data file is missing"
euler Problem {pId = 67, dataString = Just sData} =
    euler Problem {pId=18, dataString = Just sData}


-- main
--
readData :: String -> IO (Maybe String)
readData f = do
    exists <- doesFileExist f
    if exists then readFile f >>= (return . Just) else return Nothing

main = do
    args <- getArgs
    let problem_id = head args
    let dataFile = concat ["data_", problem_id, ".txt"]
    dat <- readData dataFile
    putStrLn $ show $ euler $ Problem (read $ problem_id) dat
