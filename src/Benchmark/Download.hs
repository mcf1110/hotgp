{-# LANGUAGE ScopedTypeVariables #-}

module Benchmark.Download where

import Benchmark.Core (Benchmark)
import Control.Exception (SomeException, handle)
import Control.Monad
import System.Directory
import System.IO (hFlush, stdout)
import System.Process

csvDirs :: FilePath
csvDirs = "datasets/"

downloadBenchmark :: FilePath -> String -> IO ()
downloadBenchmark workDir name = do
  createDirectoryIfMissing True $ workDir <> csvDirs
  dlAndUnzip workDir name "random"
  dlAndUnzip workDir name "edge"

dlAndUnzip :: FilePath -> String -> String -> IO ()
dlAndUnzip workDir name suffix = do
  let url = makeUrl name suffix
      fileName = workDir <> csvDirs <> jsonName name suffix
  fileExists <- doesFileExist fileName
  unless fileExists $ do
    putStrLn $ "::: Downloading " <> fileName <> " :::"
    hFlush stdout
    callCommand $ "curl -LJ -o " <> fileName <> ".gz " <> url
    let unzipAndShuffle = "gzip --to-stdout -d " <> fileName <> ".gz | sort -R > " <> fileName
    handle (\(e :: SomeException) -> return ()) $ do
      callCommand unzipAndShuffle
      removeFile $ fileName <> ".gz"
      putStrLn $ "::: Done! " <> fileName <> " :::"
      hFlush stdout

makeUrl :: String -> String -> String
makeUrl name suffix =
  "https://github.com/thelmuth/program-synthesis-benchmark-datasets/raw/master/datasets/"
    <> name
    <> "/"
    <> jsonName name suffix
    <> ".gz"

jsonName :: String -> String -> String
jsonName name suffix = name <> "-" <> suffix <> ".json"