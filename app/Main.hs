{-# LANGUAGE OverloadedStrings #-}

module Main where

import Password(confPass)

import Control.Monad(forM_)
import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.Directory(createDirectoryIfMissing, setCurrentDirectory, copyFile)
import System.FilePath((</>))
import Database.MySQL.Base(connect, defaultConnectInfo, ConnectInfo(..))

import Confluence.Database(getSpace, Space(..), Page(..), Attachment(..))
import Confluence.Format(confluenceToPandoc)

-- From Confluence.Database:
-- data Space = Space { spaceName :: String, spaceDesc :: String, spaceTopLevelPages :: [Page] }
-- data Page = Page { pageTitle :: String, pageContents :: String, pageChildren :: [Page], pageAttachments :: [Attachment] }
-- data Attachment = Attachment { attachmentName :: String, attachmentFilePath :: String }


encodeFilename :: String -> String
encodeFilename ""      = ""
encodeFilename ('/':s) = '%':'2':'F':encodeFilename s
encodeFilename (c:s)   = c:encodeFilename s

createPage :: Page -> IO ()
createPage (Page title contents children attachments) = do
  let title' = encodeFilename title
  mapM_ (\(Attachment name path) -> copyFile path ("." </> name)) attachments
  case children of
    [] -> writeFile (title' ++ ".page") $ confluenceToPandoc contents
    _  -> do
      createDirectoryIfMissing False title'
      setCurrentDirectory title'
      writeFile ("index.page") $ confluenceToPandoc contents
      mapM_ createPage children
      setCurrentDirectory ".."

getData :: String -> IO ()
getData spacekey = do
  conn <- connect defaultConnectInfo {ciUser = "confluence", ciPassword = confPass, ciDatabase = "confluence"}
  space <- getSpace conn spacekey
  case space of
    Nothing -> do
      putStrLn $ "Could not find space '" ++ spacekey ++ "'"
    Just (Space title _ tlpages) -> do
      createDirectoryIfMissing False title
      setCurrentDirectory title
      forM_ tlpages createPage
      setCurrentDirectory ".."

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Usage: confdump <space> ..."
      exitFailure
    _ -> do
      createDirectoryIfMissing False "confdumpdata"
      setCurrentDirectory "confdumpdata"
      mapM_ getData args
