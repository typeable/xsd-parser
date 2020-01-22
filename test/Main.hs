{-# OPTIONS -fno-warn-unused-imports #-}
module Main where

import Control.Exception
import Data.ByteString.Lazy as BL
import Data.Either
import Test.Hspec
import Text.XML
import Text.XSD


main :: IO ()
main = hspec $ do
  it "parses simple xsd correctly" $ do
    xsd <- parseXSD def <$> BL.readFile "test/fixtures/simple.xsd"
    xsd `shouldSatisfy` isRight
  it "parses retrieve_set_map xsd correctly" $ do
    xsd <- parseXSD def <$> BL.readFile "test/fixtures/retrieve_seat_map.xml"
    xsd `shouldSatisfy` isRight
  -- complexContent, imports and extensions are unsupported
  -- it "parses enhanced_air_ticket xsd correctly" $ do
  --   xsd <- parseXSD def <$> BL.readFile "test/fixtures/enhanced_air_ticket.xsd"
  --   xsd `shouldSatisfy` isRight
