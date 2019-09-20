{-# LANGUAGE TemplateHaskell #-}

-- | A subset of XSD specification.
module Text.XSD.Internal where

import Control.Lens
import Data.Text

type Attribute = (Text, Text)

data Element = Element
  { _attrs    :: [Attribute]
  , _elements :: [Element] }
  | Choice Element Element
  deriving (Show)

makeLenses ''Element
