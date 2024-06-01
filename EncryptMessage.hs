-- EncryptMessage.hs
module EncryptMessage where

import GenerateKeys (calculateKeys) -- Import key generation functions
import System.IO ( hSetBuffering, stdout, BufferMode(NoBuffering) )
import Data.Char (ord)

-- Encrypt a message using the public key
encryptWithPublicKey :: Integer -> Integer -> Integer -> Integer
encryptWithPublicKey n e m = m^e `mod` n

asciiStringToInt :: String -> Integer
asciiStringToInt str = foldl (\acc x -> acc * 256 + x) 0 $ map (toInteger . ord) str


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- Disable output buffering for immediate input
  putStrLn "Enter your public key (n e):"
  publicKeyInput <- getLine
  let [n, e] = map read (words publicKeyInput) :: [Integer]
  putStrLn "Enter the message:"
  messageInput <- getLine
  let m = asciiStringToInt messageInput
      encryptedMessage = encryptWithPublicKey n e m
  putStrLn $ "Encrypted message: " ++ show encryptedMessage