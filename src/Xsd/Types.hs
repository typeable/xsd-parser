{-# LANGUAGE OverloadedStrings #-}

module Xsd.Types
( Xsd(..)
, Namespace(..)
, Child(..)
, Type(..)
, SimpleType(..)
, Restriction(..)
, Constraint(..)
, ComplexType(..)
, RefOr(..)
, Element(..)
, MaxOccurs(..)
, Attribute(..)
, Annotation(..)
, Use(..)
, ModelGroup(..)
, Import(..)
, Include(..)
, QName(..)
, schemaNamespace
)
where

import Data.Text (Text)

data Xsd = Xsd
  { targetNamespace :: Maybe Namespace
  , children :: [Child]
  -- XXX: Investigate whether there could be any dependency on the order
  -- on children. E.g whether an element can reference type from other
  -- namespace only after the namespace is imported.
  -- If there is no such dependency, then get rid of Child and list
  -- types and elements separately here.
  }
  deriving (Show, Eq)

-- | Namespace name, e.g. \"http://www.w3.org/2001/XMLSchema\"
newtype Namespace = Namespace
  { fromNamespace :: Text
  }
  deriving (Show, Eq, Ord)

data Child
  = ChildElement Element
  | ChildType QName Type
  | ChildImport Import
  | ChildInclude Include
  deriving (Show, Eq)

data Annotation = Documentation Text
  deriving (Show, Eq)

data Element = Element
  { elementName :: QName
  , elementType :: RefOr Type
  , elementOccurs :: (Int, MaxOccurs)
  , elementAnnotations :: [Annotation]
  }
  deriving (Show, Eq)

data MaxOccurs
  = MaxOccurs Int
  | MaxOccursUnbound
  deriving (Show, Eq)

data Type
  = TypeSimple SimpleType
  | TypeComplex ComplexType
  deriving (Show, Eq)

data Import = Import
  { importNamespace :: Maybe Namespace
  , importLocation :: Maybe Text
  }
  deriving (Show, Eq)

data Include = Include
  { includeLocation :: Text
  }
  deriving (Show, Eq)

data SimpleType
  = AtomicType Restriction [Annotation]
  | ListType (RefOr SimpleType) [Annotation]
  -- TODO: also union
  deriving (Show, Eq)

data ComplexType = ComplexType
  { complexAttributes :: [Attribute]
  , complexModelGroup :: Maybe ModelGroup
  , complexAnnotations :: [Annotation]
  }
  deriving (Show, Eq)

data ModelGroup
  = Sequence [Element]
  | Choice [Element]
  | All [Element]
  deriving (Show, Eq)

data Attribute = Attribute
  { attrName :: QName
  , attrType :: RefOr SimpleType
  , attrUse :: Use
  }
  deriving (Show, Eq)

data Use
  = Optional
  | Prohibited
  | Required
  deriving (Show, Eq)

data Restriction = Restriction
  { restrictionBase :: RefOr SimpleType
  , restrictionConstraints :: [Constraint]
  }
  deriving (Show, Eq)

-- | Reference to a type or an inline one
data RefOr t
  = Ref QName
  | Inline t
  deriving (Show, Eq)

data Constraint
  = Enumeration Text
  deriving (Show, Eq)

data QName = QName
  { qnNamespace :: Maybe Namespace
  , qnName :: Text
  }
  deriving (Show, Eq, Ord)

-- | XMLSchema namespace, all relevant xml nodes should live in it
schemaNamespace :: Text
schemaNamespace = "http://www.w3.org/2001/XMLSchema"
