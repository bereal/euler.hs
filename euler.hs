import System.Environment
import System.Directory
import Data.List
import Data.Maybe
import Char
import Debug.Trace

-- Utils

infixl 6 ?
True ? x = const x
False ? _ = id

maximumWith :: (Ord  b) => (a->b) -> [a] -> a
maximumWith f l = 
    let pairs = [(f a, a) | a <- l]
        cmp t1 t2 = compare (fst t1) (fst t2)
    in snd $ maximumBy cmp pairs

fibonacci a b = let c = a + b in (c : fibonacci b c)

factorial n = product [2..n]

-- A primes sieve from haskell.org:

primes = 2 : eratos [3,5..]  where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p*p,p*p+2*p..])

primesTo n = takeWhile (<=n) primes

minus (x:xs) (y:ys) = case (compare x y) of
    LT -> x : minus xs (y:ys)
    EQ -> minus xs ys
    GT -> minus (x:xs) ys
minus xs _ = xs

primeFactors :: Int -> [Int]
primeFactors n = factor n $ primesTo n
  where
    factor n (p:ps) 
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      = factor n ps

--
-- Problems

data Problem = Problem { pId :: Int, dataString :: Maybe String }

euler :: Problem -> Int

euler Problem {pId = 1} = 
    let divisors = [3,5]
        limit    = 1000
        isGood n = any (==0) $ map (mod n) divisors
        goodNums = filter isGood [1..limit-1]
    in sum goodNums

euler Problem {pId = 2} = 
    let limit = 4000000
        nums = takeWhile (<=limit) (fibonacci 0 1) 
    in sum $ filter even nums

euler Problem {pId = 3}  = maximum $ primeFactors 600851475143

euler Problem {pId = 4}  =
    let palindromes = map mkPalind [999,998..100]
            where mkPalind n = read $ s ++ (reverse s) where s = show n

        -- We can use the fact that any palindrome of 2*n digits is a multiple of 11
        divisors = [990, 979..100]
        isGoodDiv n d = let (q, r) = (n `divMod` d) 
                        in (r==0 && q < 1000 && q > 100)
        isSolution n = isJust $ find (isGoodDiv n) divisors
    in head $ filter isSolution palindromes 

euler Problem {pId = 5}  = foldr1 lcm [1..20]

euler Problem {pId = 6} = 
    let nums = [1..100]
        sumSq = sum . map (^2)
        sqSum = (^2) . sum
    in (sqSum nums) - (sumSq nums)

euler Problem {pId = 7}  = head $ drop 10000 $ primes
    
euler Problem {pId = 8, dataString=Just sData} = 
    let sublists len arr =
            if length arr < len
                then []
                else (take len arr : (sublists len $ tail arr))
        digits = [digitToInt d | d <- sData, isDigit d]
    in
        maximum [product arr | arr <- sublists 5 digits]


euler Problem {pId = 9} = 
    let param = 1000
        isSolution (x, y) = x^2 + y^2 == (param - x - y)^2
        (a, b) = head $ filter isSolution [(x, y) | x <- [1..1000], y <- [1..1000-x]]
    in a * b * (param - a - b)

euler Problem {pId = 10} = sum $ primesTo 2000000

euler Problem {pId = 12} =
    let triangles = scanl1 (+) [1..]
        countDivisors n = product $ map ((+1).length) $ group $ primeFactors n
        isSolution n = countDivisors n > 500
    in head $ filter isSolution triangles

euler Problem {pId = 13, dataString = Just sData}  
    = let nums = map read $ lines sData
          s = sum nums
          digitsStr = take 10 $ show s
    in read digitsStr          

euler Problem {pId = 14}  = 
    let num = 1000000
        next n 
            | even n = n `div` 2
            | otherwise = 3 * n + 1
        seqLen buf 1 = buf
        seqLen buf n = seqLen (buf+1) (next n)
    in maximumWith (seqLen 0) [(num `div` 2)..num]

euler Problem {pId = 16} = sum $ map digitToInt $ show $ 2^1000


euler Problem {pId = 20} = sum $ map digitToInt $ show $ factorial 100


-- main

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
