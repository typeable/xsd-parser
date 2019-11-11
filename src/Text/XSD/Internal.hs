{-# LANGUAGE OverloadedStrings #-}
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
  = ComplexType (Maybe Text) [Attribute] [Annotation] (Maybe ModelGroupSchema)
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

data Annotation
  = Documentation Text
  deriving (Show, Eq)

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

fromSimpleTypeStr :: QName -> Either String SimpleAtomicType
fromSimpleTypeStr (_, "anyURI")            = Right $ STAnyURI
fromSimpleTypeStr (_, "base64Binary")      = Right $ STBase64Binary
fromSimpleTypeStr (_, "boolean")           = Right $ STBoolean
fromSimpleTypeStr (_, "date")              = Right $ STDate
fromSimpleTypeStr (_, "datetime")          = Right $ STDateTime
fromSimpleTypeStr (_, "dateTimeStamp")     = Right $ STDateTimeStamp
fromSimpleTypeStr (_, "decimal")           = Right $ STDecimal DTDecimal
fromSimpleTypeStr (_, "integer")           = Right $ STDecimal DTInteger
fromSimpleTypeStr (_, "long")              = Right $ STDecimal DTLong
fromSimpleTypeStr (_, "int")               = Right $ STDecimal DTInt
fromSimpleTypeStr (_, "short")             = Right $ STDecimal DTShort
fromSimpleTypeStr (_, "byte")              = Right $ STDecimal DTByte
fromSimpleTypeStr (_, "nonNegativeInteger")= Right $ STDecimal DTNonNegativeInteger
fromSimpleTypeStr (_, "positiveInteger")   = Right $ STDecimal DTPositiveInteger
fromSimpleTypeStr (_, "unsignedLong")      = Right $ STDecimal DTUnsignedLong
fromSimpleTypeStr (_, "unsignedInt")       = Right $ STDecimal DTUnsignedInt
fromSimpleTypeStr (_, "unsignedShort")     = Right $ STDecimal DTUnsignedShort
fromSimpleTypeStr (_, "unsignedByte")      = Right $ STDecimal DTUnsignedByte
fromSimpleTypeStr (_, "nonPositiveInteger")= Right $ STDecimal DTNonPositiveInteger
fromSimpleTypeStr (_, "negativeInteger")   = Right $ STDecimal DTNegativeInteger
fromSimpleTypeStr (_, "float")             = Right $ STFloat
fromSimpleTypeStr (_, "gDay")              = Right $ STGDay
fromSimpleTypeStr (_, "gMonth")            = Right $ STGMonth
fromSimpleTypeStr (_, "gMonthDay")         = Right $ STGMonthDay
fromSimpleTypeStr (_, "gYear")             = Right $ STGYear
fromSimpleTypeStr (_, "gYearMonth")        = Right $ STGYearMonth
fromSimpleTypeStr (_, "hexBinary")         = Right $ STHexBinary
fromSimpleTypeStr (_, "NOTATION")          = Right $ STNOTATION
fromSimpleTypeStr (_, "QName")             = Right $ STQName
fromSimpleTypeStr (_, "string")            = Right $ STString StrString
fromSimpleTypeStr (_, "normalizedString")  = Right $ STString StrNormalizedString
fromSimpleTypeStr (_, "token")             = Right $ STString StrToken
fromSimpleTypeStr (_, "language")          = Right $ STString StrLanguage
fromSimpleTypeStr (_, "Name")              = Right $ STString StrName
fromSimpleTypeStr (_, "NCName")            = Right $ STString StrNCName
fromSimpleTypeStr (_, "ENTITY")            = Right $ STString StrENTITY
fromSimpleTypeStr (_, "ID")                = Right $ STString StrID
fromSimpleTypeStr (_, "IDREF")             = Right $ STString StrIDREF
fromSimpleTypeStr (_, "NMTOKEN")           = Right $ STString StrNMTOKEN
fromSimpleTypeStr (_, "time")              = Right $ STTime
fromSimpleTypeStr (_, t)                   = Left $ "unsupported type: " <> T.unpack t

-- | ENTITIES, IDREFS and NMTOKENS are not supported right now.
data SimpleType
  = STAtomic Text SimpleAtomicType [Annotation] [Restriction]
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

toQName :: Text -> QName
toQName n =
  let splitName@(f:l) = T.split (==':') n
  in if Prelude.length splitName == 1
    then (Nothing, f)
    else (Just f, Prelude.head l)

-- | Based on https://www.w3.org/TR/xmlschema11-1/#concepts-data-model
-- Secondary and helper schema components are not supported right now.
-- data SchemaComponent
--   = SCDatatype Datatype
--   | SCElement Element
--   | SCAttribute
--   deriving (Show)

data Element = Element
  { name        :: !QName
  , xtype       :: !DatatypeRef
  , minOccurs   :: !Int
  , maxOccurs   :: !Int
  , annotations :: [Annotation]
  } deriving (Show, Eq)

type DatatypeMap = M.Map Text Datatype

-- | Embodies an XSD document
newtype XSD = XSD { unXSD :: ([Element], DatatypeMap) }
  deriving (Show, Eq)
