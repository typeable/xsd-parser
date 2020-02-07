{-# LANGUAGE OverloadedStrings #-}

module Xsd.Parser
( parse
, parseFile
, parseLazyByteString
, ParseError(..)
, Config(..)
, defaultConfig
)
where

import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Text.Read (readEither)
import Text.XML
import Text.XML.Cursor

import Xsd.Types (Xsd(Xsd))
import qualified Xsd.Types as Xsd

parseSchema :: Cursor -> P Xsd
parseSchema c = do
  let targetNamespace = Xsd.Namespace <$> anAttribute "targetNamespace" c

  children <- local (\env -> env {envTargetNamespace = targetNamespace}) $
    parseChildren c
  return Xsd
    { Xsd.targetNamespace = targetNamespace
    , Xsd.children = children
    }

parseChildren :: Cursor -> P [Xsd.Child]
parseChildren c =
  catMaybes <$> mapM parseChild (c $/ anyElement)

-- | Parse top level schema node
--
-- Returns Nothing for unknown/unsupported nodes
parseChild :: Cursor -> P (Maybe Xsd.Child)
parseChild c = do
  name <- getElementName c
  case name of
    "element" -> Just . Xsd.ChildElement <$> parseElement c
    "simpleType" -> Just <$> parseTopSimpleType c
    "complexType" -> Just <$> parseTopComplexType c
    "include" -> Just . Xsd.ChildInclude <$> parseInclude c
    "import" -> Just . Xsd.ChildImport <$> parseImport c
    _ -> return Nothing

parseInclude :: Cursor -> P Xsd.Include
parseInclude c = do
  location <- theAttribute "schemaLocation" c
  return Xsd.Include
    { Xsd.includeLocation = location
    }

parseImport :: Cursor -> P Xsd.Import
parseImport c = do
  let
    location = anAttribute "schemaLocation" c
    namespace = Xsd.Namespace <$> anAttribute "namespace" c

  tns <- asks envTargetNamespace
  case (namespace, tns) of
    (Nothing, Nothing) -> parseError c "Expected namespace"
    (Just ns1, Just ns2)
      | ns1 == ns2
      -> parseError c "Namespace equals target namespace"
    _ -> return ()

  return Xsd.Import
    { Xsd.importLocation = location
    , Xsd.importNamespace = namespace
    }

parseElement :: Cursor -> P Xsd.Element
parseElement c = handleNamespaces c $ do
  name <- theAttribute "name" c

  -- XXX: generalaze RefOr parsing
  tp <- case anAttribute "type" c of
    Just t -> Xsd.Ref <$> makeQName c t
    _ -> Xsd.Inline <$> parseType c

  minOccurs <- case anAttribute "minOccurs" c of
    Nothing -> return 1
    Just val -> case readEither (Text.unpack val) of
      Left err -> parseError c ("Can't parse minOccurs: " <> Text.pack err)
      Right v -> return v

  maxOccurs <- case anAttribute "maxOccurs" c of
    Nothing -> return (Xsd.MaxOccurs 1)
    Just "unbounded" -> return Xsd.MaxOccursUnbound
    Just val -> case readEither (Text.unpack val) of
      Left err -> parseError c ("Can't parse maxOccurs" <> Text.pack err)
      Right v -> return (Xsd.MaxOccurs v)

  annotations <- parseAnnotations c

  qname <- makeTargetQName name

  return Xsd.Element
    { Xsd.elementName = qname
    , Xsd.elementType = tp
    , Xsd.elementAnnotations = annotations
    , Xsd.elementOccurs = (minOccurs, maxOccurs)
    }

-- | Parse inline type, simple or complex
parseType :: Cursor -> P Xsd.Type
parseType c = do
  simpleTypeAxis <- makeElemAxis "simpleType"
  complexTypeAxis <- makeElemAxis "complexType"
  case (c $/ simpleTypeAxis, c $/ complexTypeAxis) of
    ([], []) -> parseError c "No type"
    ([t], []) -> Xsd.TypeSimple <$> parseSimpleType t
    ([], [t]) -> Xsd.TypeComplex <$> parseComplexType t
    _ -> parseError c "Multiple types"

-- | Top level simple type
parseTopSimpleType :: Cursor -> P Xsd.Child
parseTopSimpleType c = do
  name <- theAttribute "name" c >>= makeTargetQName
  t <- parseSimpleType c
  return $ Xsd.ChildType name (Xsd.TypeSimple t)

-- | Top level complex type
parseTopComplexType :: Cursor -> P Xsd.Child
parseTopComplexType c = do
  name <- theAttribute "name" c >>= makeTargetQName
  t <- parseComplexType c
  return $ Xsd.ChildType name (Xsd.TypeComplex t)

parseSimpleType :: Cursor -> P Xsd.SimpleType
parseSimpleType c = do
  restrictionAxis <- makeElemAxis "restriction"
  listAxis <- makeElemAxis "list"
  unionAxis <- makeElemAxis "union"
  case (c $/ restrictionAxis, c $/ listAxis, c $/ unionAxis) of
    ([e], [], []) -> Xsd.AtomicType
      <$> parseRestriction e
      <*> parseAnnotations c
    ([], [l], []) -> Xsd.ListType
      <$> parseList l
      <*> parseAnnotations c
    -- TODO implement me
    ([], [], [_]) -> parseError c "Not implemented: union"
    _ -> parseError c "Expected exactly of of restriction, list or union"

-- | Restriction for simple type
parseRestriction :: Cursor -> P Xsd.Restriction
parseRestriction c = do
  -- XXX: generalaze RefOr parsing
  tp <- case anAttribute "base" c of
    Nothing -> do
      simpleTypeAxis <- makeElemAxis "simpleType"
      case c $/ simpleTypeAxis of
        [] -> parseError c "Expected simpleType"
        [s] -> Xsd.Inline <$> parseSimpleType s
        _ -> parseError c "Multiple types"
    Just t -> Xsd.Ref <$> makeQName c t

  constraints <- parseConstrains c

  return Xsd.Restriction
    { Xsd.restrictionBase = tp
    , Xsd.restrictionConstraints = constraints
    }

parseConstrains :: Cursor -> P [Xsd.Constraint]
parseConstrains c = do
  enumerationAxis <- makeElemAxis "enumeration"
  forM (c $/ enumerationAxis) $ \e -> do
    Xsd.Enumeration <$> theAttribute "value" e

parseList :: Cursor -> P (Xsd.RefOr Xsd.SimpleType)
parseList c =
  -- XXX: generalaze RefOr parsing
  case anAttribute "itemType" c of
    Nothing -> do
      simpleTypeAxis <- makeElemAxis "simpleType"
      case c $/ simpleTypeAxis of
        [] -> parseError c "Expected simpleType"
        [s] -> Xsd.Inline <$> parseSimpleType s
        _ -> parseError c "Multiple types"
    Just t -> Xsd.Ref <$> makeQName c t

parseComplexType :: Cursor -> P Xsd.ComplexType
parseComplexType c = do
  annotations <- parseAnnotations c
  cont <- parseContent c
  return Xsd.ComplexType
    { Xsd.complexAnnotations = annotations
    , Xsd.complexContent = cont
    }

parseContent :: Cursor -> P Xsd.Content
parseContent c = do
  simpleContentAxis <- makeElemAxis "simpleContent"
  complexContentAxis <- makeElemAxis "complexContent"
  case (c $/ simpleContentAxis, c $/ complexContentAxis) of
    ([], []) -> Xsd.ContentPlain <$> parsePlainContent c
    ([s], []) -> Xsd.ContentSimple <$> parseSimpleContent s
    ([], [s]) -> Xsd.ContentComplex <$> parseComplexContent s
    _ -> parseError c
      "Expected one of simpleContent, complextContent or model group"

parsePlainContent :: Cursor -> P Xsd.PlainContent
parsePlainContent c = do
  attributes <- parseAttributes c
  model <- parseModelGroup c
  return Xsd.PlainContent
    { Xsd.plainContentModel = model
    , Xsd.plainContentAttributes = attributes
    }

parseSimpleContent :: Cursor -> P Xsd.SimpleContent
parseSimpleContent c = parseError c "not implemented"

parseComplexContent :: Cursor -> P Xsd.ComplexContent
parseComplexContent c = do
  restrictionAxis <- makeElemAxis "restriction"
  extenstionAxis <- makeElemAxis "extension"
  case (c $/ restrictionAxis, c $/ extenstionAxis) of
    ([r], []) -> parseError r "not implemented"
    ([], [e]) -> Xsd.ComplexContentExtension <$> parseComplexExtension e
    _ -> parseError c "Expected one of restriction or extension"

parseComplexExtension :: Cursor -> P Xsd.ComplexExtension
parseComplexExtension c = do
  base <- theAttribute "base" c >>= makeQName c
  attributes <- parseAttributes c
  model <- parseModelGroup c
  return Xsd.ComplexExtension
    { Xsd.complexExtensionBase = base
    , Xsd.complexExtensionModel = model
    , Xsd.complexExtensionAttributes = attributes
    }

parseModelGroup :: Cursor -> P (Maybe Xsd.ModelGroup)
parseModelGroup c = do
  sequenceAxis <- makeElemAxis "sequence"
  choiceAxis <- makeElemAxis "choice"
  allAxis <- makeElemAxis "all"
  case (c $/ sequenceAxis, c $/ choiceAxis, c $/ allAxis) of
    ([], [], []) -> return Nothing
    ([g], [], []) -> Just <$> parseSequence g
    ([], [g], []) -> Just <$> parseChoice g
    ([], [], [g]) -> Just <$> parseAll g
    _ -> parseError c "Multiple model groups"

parseSequence :: Cursor -> P Xsd.ModelGroup
parseSequence c = Xsd.Sequence <$> parseElements c

parseChoice :: Cursor -> P Xsd.ModelGroup
parseChoice c = Xsd.Choice <$> parseElements c

parseAll :: Cursor -> P Xsd.ModelGroup
parseAll c = Xsd.All <$> parseElements c

-- | Get all child elements
parseElements :: Cursor -> P [Xsd.Element]
parseElements c = do
  elementAxis <- makeElemAxis "element"
  mapM parseElement (c $/ elementAxis)

parseAttributes :: Cursor -> P [Xsd.Attribute]
parseAttributes c = do
  attrAxis <- makeElemAxis "attribute"
  mapM parseAttribute (c $/ attrAxis)

parseAttribute :: Cursor -> P Xsd.Attribute
parseAttribute c = do
  name <- theAttribute "name" c >>= makeTargetQName
  use <- parseUseAttribute c

  -- XXX: generalaze RefOr parsing
  tp <- case anAttribute "type" c of
    Nothing -> do
      simpleTypeAxis <- makeElemAxis "simpleType"
      case c $/ simpleTypeAxis of
        [s] -> Xsd.Inline <$> parseSimpleType s
        [] -> parseError c "Attribute should have type"
        _ -> parseError c "Multiple simple types"
    Just t -> Xsd.Ref <$> makeQName c t

  return Xsd.Attribute
    { Xsd.attrName = name
    , Xsd.attrUse = use
    , Xsd.attrType = tp
    }

parseUseAttribute :: Cursor -> P Xsd.Use
parseUseAttribute c = case anAttribute "use" c of
  Nothing -> return Xsd.Optional
  Just "optional" -> return Xsd.Optional
  Just "required" -> return Xsd.Required
  Just "prohibited" -> return Xsd.Prohibited
  _ -> parseError c "Unknown attribute use"

parseAnnotations :: Cursor -> P [Xsd.Annotation]
parseAnnotations c = do
  annotationAxis <- makeElemAxis "annotation"
  documentationAxis <- makeElemAxis "documentation"

  let
    documentation = c
      $/ annotationAxis
      &// documentationAxis
      &/ content
  return $ Xsd.Documentation <$> documentation

-- Section: parser entry points

-- | Specifies modes of operation
data Config = Config
  { configIgnoreSchemaNamespace :: Bool
    -- ^ if not set, parser will check that all relevant xml nodes
    -- live is the correct XMLSchema namespace
  }
  deriving (Show)

-- | Default configuration
defaultConfig :: Config
defaultConfig = Config
  { configIgnoreSchemaNamespace = False
  }

-- | Parse the whole document
--
-- Make sure the document is parsed with `psRetainNamespaces` enabled!
parse :: Config -> Document -> Either ParseError Xsd
parse conf doc = do
  let
    env = Env
      { envConfig = conf
      , envTargetNamespace = Nothing
      , envPrefixes = []
      }
    c = fromDocument doc
  runExcept $ flip runReaderT env $ handleNamespaces c $ do
    schemaAxis <- makeElemAxis "schema"
    case c $.// schemaAxis of
      [] -> parseError c "Not schema found"
      [s] -> parseSchema s
      _ -> parseError c "Multiple schemata found"

-- | Parse file
--
-- It uses the `defaultConfig`
parseFile :: FilePath -> IO (Either ParseError Xsd)
parseFile path = parse defaultConfig <$>
  Text.XML.readFile def{psRetainNamespaces=True} path

parseLazyByteString :: Lazy.ByteString -> Either ParseError Xsd
parseLazyByteString = parse defaultConfig
  . parseLBS_ def{psRetainNamespaces=True}

-- Section: parser environment

-- | Namespace prefix, e.g. \"xs\" or \"tns\"
newtype Prefix = Prefix Text
  deriving (Show, Eq)

-- | Parser environment
data Env = Env
  { envConfig :: Config
  , envTargetNamespace :: Maybe Xsd.Namespace
    -- ^ targetNamespace for the current schema
  , envPrefixes :: [(Maybe Prefix, Xsd.Namespace)]
    -- ^ all xmlns:prefix="namespace" things from the
    -- schema element
  }
  deriving (Show)

-- | Parser
type P a = ReaderT Env (Except ParseError) a

-- Section: error handling

-- | Describes what exactly went wrong
data ParseError = ParseError
  { errorLocation :: Text
    -- ^ e.g. schema.element[Person].attribute[firstName]
  , errorMessage :: Text
  }
  deriving (Show)

-- | Builds path to the current curson position
--
-- Goes through parents and collects their element names,
-- optionally adds value of "name" attribute
pprCursor :: Cursor -> Text
pprCursor = Text.intercalate "." . go []
  where
  go ctx c = case node c of
    NodeElement e ->
      let
        ctx' = name : ctx
        elemName = nameLocalName (elementName e)
        name = case laxAttribute "name" c of
          (n:_) -> elemName <> "[" <> n <> "]"
          _ -> elemName
      in case parent c of
        (p:_) -> go ctx' p
        _ -> ctx'
    _ -> ctx

-- | Fail with the location and message
parseError :: Cursor -> Text -> P a
parseError c msg = lift $ throwE ParseError
  { errorLocation = pprCursor c
  , errorMessage = msg
  }

-- Section: general utilities

-- | Get value of an optional attribute
anAttribute :: Text -> Cursor -> Maybe Text
anAttribute name c = case c $| laxAttribute name of
  (a:_) -> Just a
  _ -> Nothing

-- | Get value of the required attribute
theAttribute :: Text -> Cursor -> P Text
theAttribute name c = maybe err return (anAttribute name c)
  where
  err = parseError c ("Element should have attibute " <> name)

-- | Select nodes with the specified name
--
-- Depending on the `configIgnoreSchemaNamespace`, it will or will not
-- check the namespace to be XMLSchema one
makeElemAxis :: Text -> P Axis
makeElemAxis name = do
  ignore <- asks (configIgnoreSchemaNamespace . envConfig)
  return $ if ignore
    then laxElement name
    else element (mkSchemaName name)

-- | Parses reference to a type
--
-- Fails if it has a prefix which is not bound
makeQName :: Cursor -> Text -> P Xsd.QName
makeQName c t = case Text.splitOn ":" t of
  [n] -> do
    return Xsd.QName
      { Xsd.qnNamespace = Nothing
      , Xsd.qnName = n
      }
  [p, n] -> do
    known <- asks (lookup (Just (Prefix p)) . envPrefixes)
    case known of
      Nothing -> parseError c "Unbound prefix"
      _ -> return ()
    return Xsd.QName
      { Xsd.qnNamespace = known
      , Xsd.qnName = n
      }
  _ -> parseError c ("Can't parse name " <> t)

makeTargetQName :: Text -> P Xsd.QName
makeTargetQName name = do
  tns <- asks envTargetNamespace
  return Xsd.QName
    { Xsd.qnNamespace = tns
    , Xsd.qnName = name
    }

-- | Get name of the current node
--
-- Depending on `configIgnoreSchemaNamespace` value, it will or will not
-- check the namespace to be XMLSchema one
getElementName :: Cursor -> P Text
getElementName c = do
  case node c of
    NodeElement e -> do
      let name = elementName e
      ignore <- asks (configIgnoreSchemaNamespace . envConfig)
      unless ignore $ do
        when (nameNamespace name /= Just Xsd.schemaNamespace) $ do
          parseError c "Expected XMLSchema element"
      return (nameLocalName name)
    _ -> parseError c "Expected node element here"

-- | Attaches xml schema namespace
mkSchemaName :: Text -> Name
mkSchemaName name = Name name (Just Xsd.schemaNamespace) Nothing

-- | Add additional xmlns bindings into local scope
handleNamespaces :: Cursor -> P a -> P a
handleNamespaces c m = do
  ns <- parsePrefixes c
  local (\env -> env {envPrefixes = ns ++ envPrefixes env}) m

-- | Parse namespace bindings
--
-- Cursor should point to schema node
parsePrefixes :: Cursor -> P [(Maybe Prefix, Xsd.Namespace)]
parsePrefixes c = case node c of
  NodeElement e -> return
    . concatMap toPrefix
    . Map.toList
    . elementAttributes
    $ e
  _ -> parseError c "Expected an element"
  where
  toPrefix (n, v) = case Text.splitOn ":" (nameLocalName n) of
    ["xmlns"] -> [(Nothing, Xsd.Namespace v)]
    ["xmlns", prefix] -> [(Just (Prefix prefix), Xsd.Namespace v)]
    _ -> []
