module GenerateKeys where

import System.Random (randomRIO)

-- Miller-Rabin primality test
millerRabinPrimality :: Integer -> IO Bool
millerRabinPrimality n
  | n < 2 = return False
  | n == 2 = return True
  | even n = return False
  | otherwise = millerRabin n 100
  where
    millerRabin :: Integer -> Integer -> IO Bool
    millerRabin n s
      | s == 0 = return True
      | otherwise = do
      a <- randomRIO (2, n - 2)
      if test n a
      then millerRabin n (s - 1)
      else return False
    

test :: Integer -> Integer -> Bool
test n a = exponentWithMod a d n == 1 || test' 0
  where
    (s, d) = decompose n
    test' :: Integer -> Bool
    test' r
      | r == s = False
      | exponentWithMod a (2^r * d) n == n - 1 = True
      | otherwise = test' (r + 1)
    
          
decompose :: Integer -> (Integer, Integer)
decompose n = decompose' (n - 1) 0
  where
    decompose' d s
      | even d = decompose' (d `div` 2) (s + 1)
      | otherwise = (s, d) 

exponentWithMod :: Integer -> Integer -> Integer -> Integer
exponentWithMod b e m =
  exponentWithMod' b e m 1
  where
    exponentWithMod' :: Integer -> Integer -> Integer -> Integer -> Integer
    exponentWithMod' b e m r
      | e == 0 = r
      | even e = exponentWithMod' (b^2 `mod` m) (e `div` 2) m r
      | otherwise = exponentWithMod' b (e-1) m (r*b `mod` m)

-- Generate two large prime numbers
generatePrimes :: IO (Integer, Integer)
generatePrimes = do
  p <- generatePrime
  q <- generatePrime
  if p == q
    then generatePrimes
    else return (p, q)

-- Generate a single prime number using Miller-Rabin test
generatePrime :: IO Integer
generatePrime = do
  n <- randomRIO (2 ^ 100 :: Integer, 2 ^ 110 :: Integer)
  isPrime <- millerRabinPrimality n
  if isPrime
    then return n
    else generatePrime

-- Calculate public and private keys
calculateKeys :: IO (Integer, Integer, Integer)
calculateKeys = do
  putStrLn "Generating keys..."
  (p, q) <- generatePrimes
  let (n, phi) = calculateNAndPhi p q
      e = calculateE phi
      d = calculateD e phi
  putStrLn "Keys generated"
  return (n, e, d)

-- Helper functions for key generation
calculateNAndPhi :: Integer -> Integer -> (Integer, Integer)
calculateNAndPhi p q =
  let n = p * q
      phi = (p - 1) * (q - 1)
  in (n, phi)

calculateE :: Integer -> Integer
calculateE phi = head [x | x <- [2..phi], gcd x phi == 1]

calculateD :: Integer -> Integer -> Integer
calculateD e phi =
  let (_, d, _) = extendedEuclidean e phi
  in if d < 0
       then d + phi
       else d

extendedEuclidean :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclidean a 0 = (a, 1, 0)
extendedEuclidean a b =
  let (gcd, x', y') = extendedEuclidean b (a `mod` b)
      x = y'
      y = x' - (a `div` b) * y'
  in (gcd, x, y)

-- Check if it is safe against wiener's attack
isSafeAgainstWiener :: Integer -> Integer -> Bool
isSafeAgainstWiener n d = d >= squareRoot (squareRoot n `div` 3)

squareRoot :: Integer -> Integer
squareRoot n = floor (sqrt (fromIntegral n))



main :: IO ()
main = do
  (n, e, d) <- calculateKeys
  putStrLn $ "Your public key (n, e): (" ++ show n ++ ", " ++ show e ++ ")"
  putStrLn $ "Your private key (n, d): (" ++ show n ++ ", " ++ show d ++ ")"
  if isSafeAgainstWiener n d
    then putStrLn "Your keys are safe against Wiener's attack"
    else putStrLn "Your keys are not safe against Wiener's attack"
