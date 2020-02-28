{-# LANGUAGE OverloadedStrings #-}

module Main where

import Password(confPass)

import Data.Map(Map, empty, insert)
import Control.Monad(forM_)
import Control.Exception(catch, IOException)
import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.Directory(createDirectoryIfMissing, setCurrentDirectory, copyFile, doesFileExist, removeFile)
import System.FilePath((</>))
import Database.MySQL.Base(connect, defaultConnectInfo, ConnectInfo(..))

import Confluence.Utils(mapMaybeM)
import Confluence.Database(getSpace, Space(..), Page(..), Attachment(..))
import Confluence.Format(confluenceToPandoc)

-- From Confluence.Database:
-- data Space = Space { spaceName :: String, spaceDesc :: String, spaceTopLevelPages :: [Page] }
-- data Page = Page { pageTitle :: String, pageContents :: String, pageChildren :: [Page], pageAttachments :: [Attachment] }
-- data Attachment = Attachment { attachmentName :: String, attachmentFilePath :: String }

encodeFilename :: String -> String
encodeFilename ('_':s) = encodeFilename s -- remove leading underscores from filenames
encodeFilename s = encodeFilename' s
  where
    encodeFilename' ""      = ""
    encodeFilename' ('/':s') = '%':'2':'F':encodeFilename' s' -- uri encode '/' only
    encodeFilename' (c:s')   = c:encodeFilename' s'

copyAttachment :: String -> String -> IO ()
copyAttachment path name = do
  exists <- doesFileExist path
  if exists then
    copyFile path ("." </> name)
  else
    putStrLn $ "Could not find file: " ++ path

createPage :: Map String String -> [String] -> Page -> IO ()
createPage pm spacekeys (Page title contents children attachments) = do
  let title' = encodeFilename title
  case (title', children) of
    ("", _) -> return ()
    (_, []) -> convertAndWritePage title title'
    _       -> do
      convertAndWritePage title title'
      createDirectoryIfMissing False title'
      setCurrentDirectory title'
      mapM_ (createPage pm spacekeys) children
      setCurrentDirectory ".."
  where
    contents' = confluenceToPandoc title pm spacekeys contents
    convertAndWritePage title'' title''' = do
      mapM_ (\(Attachment name path) -> copyAttachment path name) attachments
      let filename = title''' ++ ".page"
      removeFile filename `catch` ignoreError
      -- writeFile filename $ (if title'' == title''' then "" else "---\ntitle: " ++ title'' ++ "\n...\n\n") ++ contents'
      writeFile filename $ "---\ntitle: " ++ title'' ++ "\n...\n\n" ++ contents'
    ignoreError :: IOException -> IO ()
    ignoreError _ = return ()

createPageMap :: [Page] -> Map String String
createPageMap = createPageMap' "/" empty
  where
    createPageMap' _ pm [] = pm
    createPageMap' path pm (Page title _ children _:ps) = let title' = encodeFilename title
                                                              path' = path </> title'
                                                          in  createPageMap' path (createPageMap' path' (insert title' path' pm) $ children) ps

getData :: [String] -> IO ()
getData spacekeys = do
  conn <- connect defaultConnectInfo {ciUser = "confluence", ciPassword = confPass, ciDatabase = "confluence"}
  spaces <- mapMaybeM (getSpace conn) spacekeys
  let pm = createPageMap $ concatMap spaceTopLevelPages spaces
  forM_ spaces $ \(Space title _ tlpages) -> do
    createDirectoryIfMissing False title
    setCurrentDirectory title
    forM_ tlpages (createPage pm spacekeys)
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
      getData args
