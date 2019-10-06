{-# LANGUAGE TemplateHaskell #-}

-- | A subset of XSD specification.
module Text.XSD.Internal where

import Control.Exception
import Control.Lens
import Data.Text as T
import Data.Map as M


-- | Roughly based on https://www.w3.org/TR/xmlschema11-2/#built-in-datatypes
-- | and https://www.w3.org/TR/xmlschema11-1/#dcl.ctd.ctsc
data Datatype
  = TypeComplex ComplexType
  | TypeSimple SimpleType
  deriving (Show, Eq)

data ComplexType
  = ComplexType (Maybe Text) [Attribute] (Maybe ModelGroupSchema)
  deriving (Show, Eq)

data Attribute = Attribute Text SimpleType UseProp
  deriving (Show, Eq)

data UseProp
  = Optional -- this is the default if missing in xsd
  | Prohibited
  | Required
  deriving (Show, Eq)

fromUsePropStr :: Text -> Maybe UseProp
fromUsePropStr "optional"   = Just $ Optional
fromUsePropStr "required"   = Just $ Required
fromUsePropStr "prohibited" = Just $ Prohibited
fromUsePropStr _            = Nothing

data DatatypeRef
  = InlineComplex Datatype
  | DatatypeRef Text
  deriving (Show, Eq)

-- | Based on https://www.w3.org/TR/2012/REC-xmlschema11-1-20120405/structures.html#Model_Group_details
data ModelGroupSchema
  = CTSequence [Element]
  | CTChoice [Element]
  | CTAll [Element]
  deriving (Show, Eq)

fromSimpleTypeStr :: Text -> Either String SimpleAtomicType
fromSimpleTypeStr "anyURI"             = Right $ STAnyURI
fromSimpleTypeStr "base64Binary"       = Right $ STBase64Binary
fromSimpleTypeStr "boolean"            = Right $ STBoolean
fromSimpleTypeStr "date"               = Right $ STDate
fromSimpleTypeStr "datetime"           = Right $ STDateTime
fromSimpleTypeStr "dateTimeStamp"      = Right $ STDateTimeStamp
fromSimpleTypeStr "decimal"            = Right $ STDecimal DTDecimal
fromSimpleTypeStr "integer"            = Right $ STDecimal DTInteger
fromSimpleTypeStr "long"               = Right $ STDecimal DTLong
fromSimpleTypeStr "int"                = Right $ STDecimal DTInt
fromSimpleTypeStr "short"              = Right $ STDecimal DTShort
fromSimpleTypeStr "byte"               = Right $ STDecimal DTByte
fromSimpleTypeStr "nonNegativeInteger" = Right $ STDecimal DTNonNegativeInteger
fromSimpleTypeStr "positiveInteger"    = Right $ STDecimal DTPositiveInteger
fromSimpleTypeStr "unsignedLong"       = Right $ STDecimal DTUnsignedLong
fromSimpleTypeStr "unsignedInt"        = Right $ STDecimal DTUnsignedInt
fromSimpleTypeStr "unsignedShort"      = Right $ STDecimal DTUnsignedShort
fromSimpleTypeStr "unsignedByte"       = Right $ STDecimal DTUnsignedByte
fromSimpleTypeStr "nonPositiveInteger" = Right $ STDecimal DTNonPositiveInteger
fromSimpleTypeStr "negativeInteger"    = Right $ STDecimal DTNegativeInteger
fromSimpleTypeStr "float"              = Right $ STFloat
fromSimpleTypeStr "gDay"               = Right $ STGDay
fromSimpleTypeStr "gMonth"             = Right $ STGMonth
fromSimpleTypeStr "gMonthDay"          = Right $ STGMonthDay
fromSimpleTypeStr "gYear"              = Right $ STGYear
fromSimpleTypeStr "gYearMonth"         = Right $ STGYearMonth
fromSimpleTypeStr "hexBinary"          = Right $ STHexBinary
fromSimpleTypeStr "NOTATION"           = Right $ STNOTATION
fromSimpleTypeStr "QName"              = Right $ STQName
fromSimpleTypeStr "string"             = Right $ STString StrString
fromSimpleTypeStr "normalizedString"   = Right $ STString StrNormalizedString
fromSimpleTypeStr "token"              = Right $ STString StrToken
fromSimpleTypeStr "language"           = Right $ STString StrLanguage
fromSimpleTypeStr "Name"               = Right $ STString StrName
fromSimpleTypeStr "NCName"             = Right $ STString StrNCName
fromSimpleTypeStr "ENTITY"             = Right $ STString StrENTITY
fromSimpleTypeStr "ID"                 = Right $ STString StrID
fromSimpleTypeStr "IDREF"              = Right $ STString StrIDREF
fromSimpleTypeStr "NMTOKEN"            = Right $ STString StrNMTOKEN
fromSimpleTypeStr "time"               = Right $ STTime
fromSimpleTypeStr t                    = Left $ "unsupported type: " <> T.unpack t

-- | ENTITIES, IDREFS and NMTOKENS are not supported right now.
data SimpleType
  = STAtomic SimpleAtomicType [Restriction]
  -- STUnion [SimpleType] -- special - xs:union
  -- STList [SimpleType]  -- special - xs:list
  deriving (Show, Eq)

data Restriction
  = Enumeration [Text]
  deriving (Show, Eq)

data SimpleAtomicType
  = STAnyURI
  | STBase64Binary
  | STBoolean
  | STDate
  | STDateTime
  | STDateTimeStamp
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
  | STQName
  | STString StringType
  | STTime
  deriving (Show, Eq)

data StringType
  = StrString
  | StrNormalizedString
  | StrToken
  | StrLanguage
  | StrName
  | StrNCName
  | StrENTITY
  | StrID
  | StrIDREF
  | StrNMTOKEN
  deriving (Show, Eq)

data DurationType = DayTimeDuration | YearMonthDuration deriving (Show, Eq)

data DecimalType
  = DTDecimal
  | DTInteger
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
  } deriving (Show, Eq)

-- | Embodies an XSD document
newtype XSD = XSD { unXSD :: [Element] }
  deriving (Show, Eq)
