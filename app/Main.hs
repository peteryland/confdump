{-# LANGUAGE OverloadedStrings #-}

module Main where

import Password(confPass)

import Data.Maybe(listToMaybe)
import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.Directory(createDirectory, setCurrentDirectory)
import Database.MySQL.Base(connect, defaultConnectInfo, ConnectInfo(..))

import Confluence.Database(getSpace, Space(..), Page(..))
import Confluence.Format(confluenceToPandoc)

-- From Confluence.Database:
-- data Space = Space { spaceName :: String, spaceDesc :: String, spaceHomePage :: Page }
-- data Page = Page { pageTitle :: String, pageContents :: String, pageChildren :: [Page] }

createPage :: [String] -> Page -> IO ()
createPage fs (Page title contents children) = do
  writeFile (title ++ ".page") $ confluenceToPandoc fs contents
  case children of
    [] -> return ()
    _  -> do
      createDirectory title
      setCurrentDirectory title
      mapM_ (createPage fs) children
      setCurrentDirectory ".."

getData :: String -> IO ()
getData spacekey = do
  conn <- connect defaultConnectInfo {ciUser = "confluence", ciPassword = confPass, ciDatabase = "confluence"}
  space <- getSpace conn spacekey
  case space of
    Nothing -> do
      putStrLn $ "Could not find space '" ++ spacekey ++ "'"
      exitFailure
    Just (Space title _ home) -> do
      createDirectory title
      setCurrentDirectory title
      createPage [] home

main :: IO ()
main = do
  args <- getArgs
  case listToMaybe args of
    Just space ->
      getData space
    Nothing -> do
      putStrLn "Usage: confdump <space>"
      exitFailure
