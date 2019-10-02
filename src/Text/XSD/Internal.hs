{-# LANGUAGE TemplateHaskell #-}

-- | A subset of XSD specification.
module Text.XSD.Internal where

import Control.Lens
import Data.Text
import Data.Map as M


-- | Roughly based on https://www.w3.org/TR/xmlschema11-2/#built-in-datatypes
data Datatype
  = ComplexType ComplexType
  | SimpleType SimpleType
  deriving (Show)

-- | Based on https://www.w3.org/TR/xmlschema11-1/#dcl.ctd.ctsc
-- Only sequence and choice are supported
data ComplexType
  = CTSequence
  | CTChoice
  deriving (Show)

-- | ENTITIES, IDREFS and NMTOKENS are not supported right now.
data SimpleType
  = STAnyURI
  | STBase64Binary
  | STBoolean
  | STDate
  | STDateTime
  | STDecimal DecimalType
  | STDouble
  | STDuration DurationType
  | STFloat
  | STGDay
  | STGMonth
  | STGMonthDay
  | STGYear
  | STGYearMonth
  | STHexBinary
  | STNOTATION
  | STQName QName
  | STString StringType
  | STTime
  | STUnion -- special - xs:union
  | STList  -- special - xs:list
  deriving (Show)

data StringType
  = StrNormalizedString
  | StrToken
  | StrLanguage
  | StrName
  | StrNCNAME
  | StrENTITY
  | StrID
  | StrIDREF
  | StrNMTOKEN
  deriving (Show)

data DurationType = DayTimeDuration | YearMonthDuration deriving (Show, Eq)

data DecimalType
  = DTInteger
  | DTLong
  | DTInt
  | DTShort
  | DTByte
  | DTNonNegativeInteger
  | DTPositiveInteger
  | DTUnsignedLong
  | DTUnsignedInt
  | DTUnsignedShort
  | DTUnsignedByte
  | DTNonPositiveInteger
  | DTNegativeInteger
  deriving (Show, Eq)

type LocalName = Text

type Namespace = Text

type QName = (Namespace, LocalName)

-- data Definition
--   = XSString
--   | XSSeq [Definition]
--   | XSChoice [Definition]
--   deriving (Show)

type Attribute = (Text, Text)

-- | Based on https://www.w3.org/TR/xmlschema11-1/#concepts-data-model
-- Secondary and helper schema components are not supported right now.
data SchemaComponent
  = SCSimpleType SimpleType
  | SCComplexType ComplexType
  | SCElement Element
  | SCAttribute
  deriving (Show)

data Element = Element
  { name       :: String
  , target     :: Maybe Namespace
  , xtype      :: Datatype
  , attrs      :: [Attribute]
  , abstract   :: Bool
  , elements :: [Element]
  }
  deriving (Show)

-- | Embodies an XSD document
newtype XSD = XSD { unXSD :: Element }
  deriving (Show)
