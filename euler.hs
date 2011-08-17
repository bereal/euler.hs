import System.Environment
import Data.List
import Data.Maybe

-- Utils

fibonacci a b = let c = a + b in (c : fibonacci b c)


primesTo m = 2 : eratos [3,5..m]  where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p,p+2*p..m])

minus (x:xs) (y:ys) = case (compare x y) of
    LT -> x : minus xs (y:ys)
    EQ -> minus xs ys
    GT -> minus (x:xs) ys
minus xs _ = xs

-- Problems

euler :: Int -> Int

euler 1 = let divisors = [3,5]
              limit    = 1000
              isGood n = any (==0) $ map (mod n) divisors
              goodNums = filter isGood [1..limit-1]
          in sum goodNums

euler 2 = 
    let limit = 4000000
        nums = takeWhile (<=limit) (fibonacci 0 1) 
    in sum $ filter even nums

euler 3 =
    let param = 600851475143
        maxDiv :: Int -> [Int] -> Int
        maxDiv n (p:ps) = case (n `divMod` p) of
            (1, 0) -> p
            (q, 0) -> maxDiv q (p:ps)
            _ -> maxDiv n ps
    in maxDiv param $ primesTo param

euler 4 =
    let palindromes = map mkPalind [999,998..100]
            where mkPalind n = read $ s ++ (reverse s) where s = show n

        -- We can use the fact that any palindrome of 2*n digits is a multiple of 11
        divisors = [990, 979..100]
        isGoodDiv n d = let (q, r) = (n `divMod` d) 
                        in (r==0 && q < 1000 && q > 100)
        isSolution n = isJust $ find (isGoodDiv n) divisors
    in fromJust $ find isSolution palindromes

euler 5 = foldr1 lcm [1..20]

euler 6 = 
    let nums = [1..100]
        sumSq = sum . map (^2)
        sqSum = (^2) . sum
    in (sqSum nums) - (sumSq nums)

euler 7 = head $ drop 10000 $ primesTo 100000000000
    
euler 8 = 
    let sublists len arr =
            if length arr < len
                then []
                else (take len arr : (sublists len $ tail arr))
        bignum = concat [
            "73167176531330624919225119674426574742355349194934",
            "96983520312774506326239578318016984801869478851843",
            "85861560789112949495459501737958331952853208805511",
            "12540698747158523863050715693290963295227443043557",
            "66896648950445244523161731856403098711121722383113",
            "62229893423380308135336276614282806444486645238749",
            "30358907296290491560440772390713810515859307960866",
            "70172427121883998797908792274921901699720888093776",
            "65727333001053367881220235421809751254540594752243",
            "52584907711670556013604839586446706324415722155397",
            "53697817977846174064955149290862569321978468622482",
            "83972241375657056057490261407972968652414535100474",
            "82166370484403199890008895243450658541227588666881",
            "16427171479924442928230863465674813919123162824586",
            "17866458359124566529476545682848912883142607690042",
            "24219022671055626321111109370544217506941658960408",
            "07198403850962455444362981230987879927244284909188",
            "84580156166097919133875499200524063689912560717606",
            "05886116467109405077541002256983155200055935729725",
            "71636269561882670428252483600823257530420752963450"]
        digits :: [Int]
        digits = [read [d] | d <- bignum]
    in
        foldr1 max [foldr1 (*) arr | arr <- sublists 5 digits]

-- main

main = do
    problem_id <- getArgs 
    putStrLn $ show $ euler $ read $ head problem_id
