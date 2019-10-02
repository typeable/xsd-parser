module Text.XSD.Parser where

import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans.Reader
import Data.ByteString.Lazy
import Data.Map as M
import Data.Text
import Prelude as P
import Text.XML as XML
import Text.XML.Cursor as XML
import Text.XSD.Internal as XSD


newtype XSDException = XSDException String
  deriving (Show)

instance Exception XSDException

type XSDMonad a = ReaderT (M.Map Name Datatype) (Either SomeException) a

parseXSD :: ParseSettings -> ByteString -> Either SomeException XSD.Element
parseXSD ps bs = parseLBS ps bs >>= toXsd

data Expectation
  = ExpName Text
  | Expectation :+: Expectation

satisfy :: Expectation -> XML.Node -> Bool
satisfy exp node@(NodeElement (XML.Element name _ _)) =
  case node of
    NodeElement (XML.Element (Name name _ _) _ _) -> case exp of
      ExpName n     -> name == n
      exp1 :+: exp2 -> satisfy exp1 node && satisfy exp2 node

satisfySome
  :: XML.Cursor
  -> [Expectation]
  -> Bool
satisfySome c e = satisfySome' True c e

satisfySome'
  :: Bool -- ^ check current
  -> XML.Cursor
  -> [Expectation]
  -> Bool
satisfySome' checkCurrent cur []         = True
satisfySome' checkCurrent cur (exp:exps) = result
  where
    satisfyCurrent = satisfy exp (node cur)
    validChildren  = P.filter (satisfy (P.head exps) . node) (child cur)
    result         =
      if checkCurrent && P.null exps
      then satisfyCurrent
      else P.foldr (\c r -> r && satisfySome' False c exps) True validChildren

-- Show is Meh
throwXsd :: Show s => s -> XSDMonad a
throwXsd = throwError . SomeException . XSDException . show

toSchema :: XML.Node -> [XSD.Element] -> XSDMonad XSD
toSchema (XML.NodeElement (XML.Element (Name name _ _) _ _)) elems
  | name == "schema" = pure $ XSD $ XSD.Element (XSSeq []) "schema" [] elems
  | otherwise        = throwXsd "Top-level schema definition not found"

fromElem :: XML.Cursor -> XSDMonad XSD.Element
fromElem cursor = do
  case node cursor of
    NodeElement el -> case nameLocalName (elementName el) of
      "element"    -> do

      "simpleType" -> error "TODO: implement"
    e              ->
      throwXsd $ "xsd node not supported: " <> show e

toXsd :: Document -> Either SomeException XSD.Element
toXsd doc = do
  let
    cursor = fromDocument doc
    elems  = child cursor &| fromElem
  runReader M.null $ toSchema (node cursor) elems
