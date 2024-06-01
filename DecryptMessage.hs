module DecryptMessage where

import GenerateKeys (calculateKeys) -- Import key generation functions

-- Performs fast exponentiation with modulo 
exponentWithMod :: Integer -> Integer -> Integer -> Integer
exponentWithMod b e m =
  exponentWithMod' b e m 1
  where
    exponentWithMod' :: Integer -> Integer -> Integer -> Integer -> Integer
    exponentWithMod' b e m r
      | e == 0 = r
      | even e = exponentWithMod' (b^2 `mod` m) (e `div` 2) m r
      | otherwise = exponentWithMod' b (e-1) m (r*b `mod` m)

-- Decrypt a message using the private key
decryptWithPrivateKey :: Integer -> Integer -> Integer -> Integer
decryptWithPrivateKey n d c = exponentWithMod c d n

intToAsciiString :: Integer -> String
intToAsciiString n
  | n < 256 = [toEnum (fromIntegral n)]
  | otherwise = intToAsciiString (n `div` 256) ++ [toEnum (fromIntegral (n `mod` 256))]
 

main :: IO ()
main = do
  putStrLn "Enter your private key (n d):"
  privateKeyInput <- getLine
  let [n, d] = map read (words privateKeyInput) :: [Integer]
  putStrLn "Enter the encrypted message (as an integer):"
  encryptedMessageInput <- getLine
  let c = read encryptedMessageInput :: Integer
      decryptedMessage = decryptWithPrivateKey n d c
  putStrLn $ "Decrypted message: " ++ intToAsciiString decryptedMessage