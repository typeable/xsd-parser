{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.XSD.Parser where

import Control.Exception
import Control.Monad
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Reader
import Control.Monad.Writer.CPS
import Data.ByteString.Lazy
import Data.Foldable as F
-- import Data.Generics.Uniplate.Operations
import Data.Map as M
import Debug.Trace
import Data.Maybe
import Data.Text as T
import Prelude as P
import Text.XML as XML
import Text.XML.Cursor as XML
import Text.XSD.Internal as XSD


newtype XSDException = XSDException String
  deriving (Show)

instance Exception XSDException

type XSDMonad a = WriterT DatatypeMap (Either SomeException) a

parseXSD :: ParseSettings -> ByteString -> Either SomeException XSD
parseXSD ps bs = parseLBS ps bs >>= toXsd

-- Show is Meh
throwXsd :: Show s => s -> XSDMonad a
throwXsd = throwError . SomeException . XSDException . show

maybeThrowXsd :: Show s => Maybe a -> s -> XSDMonad a
maybeThrowXsd (Just a) _ = pure a
maybeThrowXsd Nothing  e = throwXsd e

attrThrowXsd :: Show s => XML.Name -> XML.Element -> s -> XSDMonad Text
attrThrowXsd name elNode = maybeThrowXsd (M.lookup name $ elementAttributes elNode)

-- | Tells the writer about named complex types and returns a list of elements.
toElemsAndDatatypes :: XML.Cursor -> XSDMonad [XSD.Element]
toElemsAndDatatypes cursor = do
  elems <- traverse toElem (cursor $/ laxElement "element")
  datatypes <- traverse toComplexType (cursor $/ laxElement "complexType")
  for_ datatypes $ \case
    datatype@(ComplexType (Just name) _ _) ->
      tell (M.singleton name (TypeComplex datatype))
    datatype@(ComplexType Nothing _  _) ->
      throwXsd $ "unnamed type ref for datatype: " <> show datatype
  pure elems

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a

-- | Takes a cursor to the element itself.
toElem :: Cursor -> XSDMonad XSD.Element
toElem cursor = do
  case node cursor of
    NodeElement el -> case nameLocalName (elementName el) of
      "element"    -> do
        let
          attrs     = elementAttributes el
          mAttrType = M.lookup "type" attrs
          ns        = nameNamespace $ elementName el
        name <- attrThrowXsd "name" el
          $ "Expected attribute 'name' for element: " <> show (node cursor)
        let
          minOc = maybe 0 (read . T.unpack) (M.lookup "minOccurs" attrs)
          maxOc = read . T.unpack <$> M.lookup "maxOccurs" attrs
        datatypeRef <- case mAttrType of
          Just dtn -> pure $ DatatypeRef dtn
          Nothing  -> do
            let complexType = cursor $// laxElement "complexType"
            if P.null complexType
            then throwXsd $ "No datatype definition found for element: " <> name
            else do
              childCursor <- maybeThrowXsd (safeHead complexType)
                $ "More than one xs:complexType tag in: " <> show cursor
              let name = safeHead $ childCursor $| attribute "name"
              InlineComplex . TypeComplex <$> toComplexType childCursor
        pure $ XSD.Element
          { name      = (ns, name)
          , xtype     = datatypeRef
          , minOccurs = minOc
          , maxOccurs = maxOc }
      e            -> throwXsd $ "[toElem] xsd node not supported: " <> show e
    e              ->
      throwXsd $ "[toElem] element expected, got: " <> show e

-- toDatatype :: Cursor -> XSDMonad Datatype
-- toDatatype cursor = do
--   case node cursor of
--     NodeElement el -> case nameLocalName (elementName el) of
--       "simpleType"  -> TypeSimple <$> toSimpleType cursor
--       "complexType" -> TypeComplex <$> toComplexType cursor
--       e             -> throwXsd $ "[toDatatype] xsd node not supported: " <> show e
--     e              ->
--       throwXsd $ "[toDatatype] element expected, got: " <> show e

toSimpleType :: Cursor -> XSDMonad SimpleType
toSimpleType cursor = do
  let restrictionAxis = laxElement "restriction"
  if P.null (cursor $// restrictionAxis)
  then throwXsd $ "unsupported simpleType element: " <> show cursor
  else do
    childCursor <- maybeThrowXsd (safeHead $ cursor $// restrictionAxis)
      $ "Unexpected number of <restriction> tags in: " <> show cursor
    baseType <- maybeThrowXsd (safeHead $ childCursor $| attribute "base")
      $ "No base attribute provided for restriction tag in: " <> show cursor
    let
      enumValues = childCursor $// laxElement "enumeration" &/ attribute "value"
    -- only enumeration is supported now
    case fromSimpleTypeStr baseType of
      Left e                  -> throwXsd e
      Right simpleAtomicType  ->
        pure $ STAtomic simpleAtomicType
          $ if P.null enumValues then [] else [Enumeration enumValues]

-- | Takes a cursor to the 'xs:complexType' element.
toComplexType :: Cursor -> XSDMonad ComplexType
toComplexType cursor = do
  let attributeAxis = laxElement "attribute"
  let sequenceAxis  = laxElement "sequence"
  let choiceAxis    = laxElement "choice"
  let allAxis       = laxElement "all"
  -- attributes
  attrs <- traverse toAttribute (cursor $// attributeAxis)
  -- group model schemas
  if P.null (cursor $// sequenceAxis)
  then if P.null (cursor $// choiceAxis)
    then if P.null (cursor $// allAxis)
      then throwXsd $ "No definition found for complexType: "
        <> show (safeHead $ cursor $// laxElement "complexType")
      else do
        childCursor <- maybeThrowXsd (safeHead $ cursor $// allAxis)
          $ "Unexpected number of xs:all tags in: " <> show cursor
        let name = safeHead $ cursor $| attribute "name"
        ComplexType name attrs . Just . CTAll <$> toElemsAndDatatypes childCursor
    else do
      childCursor <- maybeThrowXsd (safeHead $ cursor $// choiceAxis)
        $ "Unexpected number of xs:choice tags in: " <> show cursor
      let name = safeHead $ cursor $| attribute "name"
      ComplexType name attrs . Just . CTChoice <$> toElemsAndDatatypes childCursor
  else do
    childCursor <- maybeThrowXsd (safeHead $ cursor $// sequenceAxis)
      $ "Unexpected number of xs:sequence tags in: " <> show cursor
    let name = safeHead $ cursor $| attribute "name"
    ComplexType name attrs . Just . CTSequence <$> toElemsAndDatatypes childCursor

toAttribute :: XML.Cursor -> XSDMonad Attribute
toAttribute cursor = do
  name <- maybeThrowXsd (safeHead $ cursor $| attribute "name")
    $ "No name attribute found in child <attribute> of: "
      <> show (cursor $| ancestor <=< ancestor)
  simpleType <- case safeHead $ cursor $| attribute "type" of
    Just simpleAtomicType -> case fromSimpleTypeStr simpleAtomicType of
      Left e                  -> throwXsd e
      Right simpleAtomicType  -> pure $ STAtomic simpleAtomicType []
    Nothing -> do
      let simpleTypeCursor = safeHead $ cursor $// laxElement "simpleType"
      case simpleTypeCursor of
        Just st -> toSimpleType cursor
        Nothing -> throwXsd
          $ "No type attribute found in child <attribute> of: "
            <> show (cursor $| ancestor <=< ancestor)
  let
    useProp = case fromUsePropStr =<< safeHead (cursor $| attribute "use") of
      Just up -> up
      Nothing -> Optional
  pure $ Attribute name simpleType useProp

toXsd :: Document -> Either SomeException XSD
toXsd doc = do
  let cursor = fromDocument doc
  XSD <$> (runWriterT $ do
    let topLevel = cursor $| laxElement "schema"
    if P.null topLevel
    then throwXsd "Top-level schema definition not found"
    else toElemsAndDatatypes cursor)
