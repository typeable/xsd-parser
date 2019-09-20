module Text.XSD.Parser where

import Control.Exception
import Control.Monad
import Data.ByteString.Lazy
import Text.XML
import Text.XSD.Internal as XSD


toXsd :: Document -> Either SomeException XSD.Element
toXsd = undefined

parseXSD :: ParseSettings -> ByteString -> Either SomeException XSD.Element
parseXSD ps bs = toXsd =<< parseLBS ps bs
