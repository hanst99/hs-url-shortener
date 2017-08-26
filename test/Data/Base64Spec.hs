{-# LANGUAGE OverloadedStrings #-}

module Data.Base64Spec(spec)
where

import Data.Base64
import Test.Hspec
import Test.QuickCheck
import Numeric.Natural
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "Base64" $ do
    it "contains only valid characters after encoding" $ property $
      \x -> T.all isValidBase64Char $ unsafeToBase64 (fromIntegral (x :: Natural))
    it "should work with 64" $ do
        unsafeToBase64 64 `shouldBe` "10"
        unsafeFromBase64 "10" `shouldBe` 64
    it "can be decoded to the same number" $ property $
      \x -> (unsafeFromBase64 . unsafeToBase64 $ fromIntegral x) == fromIntegral (x::Natural)
  return ()
