{-# LANGUAGE TemplateHaskell #-}

-- | A subset of XSD specification.
module Text.XSD.Internal where

import Control.Lens
import Data.Text
import Data.Map as M


-- | Roughly based on https://www.w3.org/TR/xmlschema11-2/#built-in-datatypes
-- | and https://www.w3.org/TR/xmlschema11-1/#dcl.ctd.ctsc
data Datatype
  = ComplexType (Maybe Text) ModelGroupSchema [Element]
  | SimpleType SimpleType
  deriving (Show)

data DatatypeRef
  = InlineComplex Datatype
  | DatatypeRef Text
  deriving (Show)

-- | Based on https://www.w3.org/TR/2012/REC-xmlschema11-1-20120405/structures.html#Model_Group_details
data ModelGroupSchema
  = CTSequence
  | CTChoice
  | CTAll
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
  | STUnion [SimpleType] -- special - xs:union
  | STList [SimpleType]  -- special - xs:list
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

type QName = (Maybe Namespace, LocalName)

-- | Based on https://www.w3.org/TR/xmlschema11-1/#concepts-data-model
-- Secondary and helper schema components are not supported right now.
-- data SchemaComponent
--   = SCDatatype Datatype
--   | SCElement Element
--   | SCAttribute
--   deriving (Show)

data Element = Element
  { name     :: QName
  , xtype    :: DatatypeRef
  -- , elements :: [Element]
  } deriving (Show)

-- | Embodies an XSD document
newtype XSD = XSD { unXSD :: [Element] }
  deriving (Show)
