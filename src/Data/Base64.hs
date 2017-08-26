{-# LANGUAGE OverloadedStrings #-}
module Data.Base64 where

import Data.Text(Text)
import qualified Data.Text as T
import Data.Char(ord, chr)

base64ValidChars :: [Char]
base64ValidChars = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ ['-', '_']

isValidBase64Char :: Char -> Bool
isValidBase64Char = (`elem` base64ValidChars)

unsafeToBase64 :: Integer -> Text
unsafeToBase64 n' = T.reverse $ unsafeToBase64' n' where
  unsafeToBase64' n | n > 0 = T.cons (encodeChar (n `mod` 64)) (unsafeToBase64' (n `div` 64))
                    | n == 0 = T.empty
                    | otherwise = error "Got negative number for unsafeToBase64"



decodeChar :: Char -> Integer
decodeChar c | c >= '0' && c <= '9' = fromIntegral $ (ord c - ord '0')
             | c >= 'A' && c <= 'Z' = fromIntegral $ (ord c - ord 'A') + 10
             | c >= 'a' && c <= 'z' = fromIntegral $ (ord c - ord 'a') + 36
             | c == '-' = 62
             | c == '_' = 63
             | otherwise = error $ "Not a valid base64 character: " ++ ['\'',c,'\'']

encodeChar :: Integer -> Char
encodeChar n | n >= 0 && n < 10 = chr (fromIntegral n + ord '0')
             | n >= 10 && n < 36 = chr (fromIntegral n - 10 + ord 'A')
             | n >= 36 && n < 62 = chr (fromIntegral n - 36 + ord 'a')
             | n == 62 = '-'
             | n == 63 = '_'
             | otherwise = error $ "Got integer larger than 63 or smaller than 0 for encodeChar"


unsafeFromBase64 :: Text -> Integer
unsafeFromBase64 = snd . T.foldl' convChar (0::Int, 0) where
  convChar (pos, num) c = (pos + 1, 64*num + decodeChar c)
