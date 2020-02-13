{-# LANGUAGE OverloadedStrings #-}

module Main
( main
)
where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.IORef
import Control.Monad
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import qualified System.IO as IO
import Text.XML
import Text.XML.Cursor

import qualified Xsd

data Stat = Stat
  { statTestSet :: Text
  , statTotal :: Int
  , statSucceeded :: Int
  }
  deriving (Show)

{-# NOINLINE errRef #-}
errRef :: IORef Int
errRef = unsafePerformIO (newIORef 0)

{-# NOINLINE okRef #-}
okRef :: IORef Int
okRef = unsafePerformIO (newIORef 0)

{-# NOINLINE statsRef #-}
statsRef :: IORef [Stat]
statsRef = unsafePerformIO (newIORef [])

dumpResults :: [Stat] -> IO ()
dumpResults stats = IO.withFile "xsts.md" IO.WriteMode $ \h -> do
  IO.hPutStrLn h "This table is generated automatically by `xsts` test."
  IO.hPutStrLn h "It contains number of xsd files successfully parsed."
  IO.hPutStrLn h "Note that we don't currently attempt to validate xml files"
  IO.hPutStrLn h "against the schemata."
  IO.hPutStrLn h ""
  IO.hPutStrLn h "Test set | Total tests | Passed tests"
  IO.hPutStrLn h "--- | --- | ---"
  forM_ stats $ \s -> do
    IO.hPutStrLn h $ mconcat
      [ Text.unpack (statTestSet s), " | "
      , show (statTotal s), " | "
      , show (statSucceeded s)
      ]

main :: IO ()
main = do
  let path = "./xsdtests/suite.xml"
  doc <- Text.XML.readFile def path
  runSuite path (fromDocument doc)
  oks <- readIORef okRef
  readIORef statsRef >>= dumpResults
  putStrLn $ "Succeeded: " ++ show oks
  errs <- readIORef errRef
  putStrLn $ "Failed: " ++ show errs
  unless (errs == 11468) $
    fail $ "There were " ++ show errs ++ " errors"

runSuite :: FilePath -> Cursor -> IO ()
runSuite path c = mapM_ (runTestSetRef dir) (c $/ laxElement "testSetRef")
  where
  dir = takeDirectory path

runTestSetRef :: FilePath -> Cursor -> IO ()
runTestSetRef dir c = do
  ref <- theAttribute c "href"
  let path = normalise (dir </> Text.unpack ref)
  runTestSet path

runTestSet :: FilePath -> IO ()
runTestSet path = do
  doc <- Text.XML.readFile def path
  let c = fromDocument doc
  name <- theAttribute c "name"
  putStrLn ("----- Test set " <> Text.unpack name)
  results <- mapM (runTestGroup dir) (c $/ laxElement "testGroup")
  let
    stat = Stat
      { statTestSet = name
      , statTotal = (sum . map fst) results
      , statSucceeded = (sum . map snd) results
      }
  modifyIORef' statsRef (stat :)
  where
  dir = takeDirectory path

runTestGroup :: FilePath -> Cursor -> IO (Int, Int)
runTestGroup dir c = do
  name <- theAttribute c "name"
  results <- mapM (runSchemaTest dir name) (aChild c "schemaTest")
  case join results of
    Nothing -> return (0, 0)
    Just True -> return (1, 1)
    Just False -> return (1, 0)

runSchemaTest :: FilePath -> Text -> Cursor -> IO (Maybe Bool)
runSchemaTest dir groupName c = do
  path <- do
    c' <- theChild c "schemaDocument"
    theAttribute c' "href"
  mvalid <- do
    c' <- theChild c "expected"
    valid <- theAttribute c' "validity"
    case valid of
      "valid" -> return (Just True)
      "invalid" -> return (Just False)
      _ -> return Nothing
  case mvalid of
    Nothing -> return Nothing -- indetermined
    Just valid -> do
      doc <- Text.XML.readFile def (dir </> Text.unpack path)
      case (Xsd.parse Xsd.defaultConfig doc, valid) of
        (Left _, False) -> do
          modifyIORef' okRef succ
          return (Just True)
        (Right _, True) -> do
          modifyIORef' okRef succ
          return (Just True)
        (Right _, False) -> do
          modifyIORef' errRef succ
          putStrLn $ "invalid xsd passed: " <> Text.unpack groupName
            <> " (" <> dir </> Text.unpack path <> ")"
          return (Just False)
        (Left err, True) -> do
          modifyIORef' errRef succ
          putStrLn $ "valid xsd didn't pass: " <> Text.unpack groupName
            <> " (" <> dir </> Text.unpack path <> ")"
            <> ": " <> show err
          return (Just False)

anAttribute :: Cursor -> Text -> Maybe Text
anAttribute c name = case c $| laxAttribute name of
  (a:_) -> Just a
  _ -> Nothing

theAttribute :: Cursor -> Text -> IO Text
theAttribute c name = maybe err return (anAttribute c name)
  where
  err = fail $ "no attribute " <> Text.unpack name

aChild :: Cursor -> Text -> Maybe Cursor
aChild c name = case c $/ laxElement name of
  (a:_) -> Just a
  _ -> Nothing

theChild :: Cursor -> Text -> IO Cursor
theChild c name = case aChild c name of
  Just a -> return a
  _ -> fail $ "no element " <> Text.unpack name
