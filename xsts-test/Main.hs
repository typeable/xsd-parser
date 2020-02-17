{-# LANGUAGE OverloadedStrings #-}

module Main
( main
)
where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.IORef
import Control.Monad
import Control.Exception
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import qualified System.IO as IO
import Text.XML
import Text.XML.Cursor

import qualified Xsd

data Stat = Stat
  { statTotal :: Int
  , statSucceeded :: Int
  , statValidFailed :: Int
  , statInvalidSucceeded :: Int
  }
  deriving (Show)

{-# NOINLINE errRef #-}
errRef :: IORef Int
errRef = unsafePerformIO (newIORef 0)

{-# NOINLINE okRef #-}
okRef :: IORef Int
okRef = unsafePerformIO (newIORef 0)

{-# NOINLINE statsRef #-}
statsRef :: IORef [(Text, Stat)]
statsRef = unsafePerformIO (newIORef [])

dumpResults :: [(Text, Stat)] -> IO ()
dumpResults stats = IO.withFile "xsts.md" IO.WriteMode $ \h -> do
  IO.hPutStrLn h "This table is generated automatically by `xsts` test."
  IO.hPutStrLn h "It contains number of xsd files successfully parsed."
  IO.hPutStrLn h "Note that we don't currently attempt to validate xml files"
  IO.hPutStrLn h "against the schemata."
  IO.hPutStrLn h ""
  IO.hPutStrLn h "Test set | Total tests | Passed tests | Invalid passed | Valid failed"
  IO.hPutStrLn h "--- | --- | --- | --- | ---"
  forM_ stats $ \(name, s) -> do
    IO.hPutStrLn h $ mconcat
      [ Text.unpack name, " | "
      , show (statTotal s), " | "
      , show (statSucceeded s), " | "
      , show (statInvalidSucceeded s), " | "
      , show (statValidFailed s)
      ]

expectedFailures :: Int
expectedFailures = 3513

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
  unless (errs == expectedFailures) $
    fail $ "There were " ++ show errs ++ " errors, expected "
      ++ show expectedFailures

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
      { statTotal = (sum . map statTotal) results
      , statSucceeded = (sum . map statSucceeded) results
      , statValidFailed = (sum . map statValidFailed) results
      , statInvalidSucceeded = (sum . map statInvalidSucceeded) results
      }
  modifyIORef' statsRef ((name, stat) :)
  where
  dir = takeDirectory path

runTestGroup :: FilePath -> Cursor -> IO Stat
runTestGroup dir c = do
  name <- theAttribute c "name"
  results <- mapM (runSchemaTest dir name) (aChild c "schemaTest")
  case join results of
    Nothing -> return Stat
      { statTotal = 0
      , statSucceeded = 0
      , statValidFailed = 0
      , statInvalidSucceeded = 0
      }
    Just ValidSucceeded -> return Stat
      { statTotal = 1
      , statSucceeded = 1
      , statValidFailed = 0
      , statInvalidSucceeded = 0
      }
    Just ValidFailed -> return Stat
      { statTotal = 1
      , statSucceeded = 0
      , statValidFailed = 1
      , statInvalidSucceeded = 0
      }
    Just InvalidSucceeded -> return Stat
      { statTotal = 1
      , statSucceeded = 0
      , statValidFailed = 0
      , statInvalidSucceeded = 1
      }
    Just InvalidFailed -> return Stat
      { statTotal = 1
      , statSucceeded = 1
      , statValidFailed = 0
      , statInvalidSucceeded = 0
      }

data Result
  = InvalidFailed
  | InvalidSucceeded
  | ValidFailed
  | ValidSucceeded
  deriving (Show)

runSchemaTest :: FilePath -> Text -> Cursor -> IO (Maybe Result)
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
      res <- try (Xsd.getSchema (dir </> Text.unpack path))
      case (res, valid) of
        (Left _, False) -> do
          modifyIORef' okRef succ
          return (Just InvalidFailed)
        (Right _, True) -> do
          modifyIORef' okRef succ
          return (Just ValidSucceeded)
        (Right _, False) -> do
          modifyIORef' errRef succ
          putStrLn $ "invalid xsd passed: " <> Text.unpack groupName
            <> " (" <> dir </> Text.unpack path <> ")"
          return (Just InvalidSucceeded)
        (Left err, True) -> do
          modifyIORef' errRef succ
          putStrLn $ "valid xsd didn't pass: " <> Text.unpack groupName
            <> " (" <> dir </> Text.unpack path <> ")"
            <> ": " <> show (err :: SomeException)
          return (Just ValidFailed)

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
