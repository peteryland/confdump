{-# LANGUAGE OverloadedStrings #-}

module Main where

import Password(confPass)

import Data.Char(isSpace)
import Data.List(intercalate)
import Data.Map(Map, empty, insert, fromList)
import Control.Monad(forM_)
import Control.Exception(catch, IOException)
import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.Directory(createDirectoryIfMissing, setCurrentDirectory, copyFile, doesFileExist, removeFile)
import System.FilePath((</>))
import Database.MySQL.Base(connect, defaultConnectInfo, ConnectInfo(..))

import Confluence.Utils(mapMaybeM, toIdent)
import Confluence.Database(getSpace, Space(..), Page(..), Attachment(..))
import Confluence.Format(confluenceToPandoc, getHeaders)

import Debug.Trace

-- From Confluence.Database:
-- data Space = Space { spaceKey :: String, spaceName :: String, spaceDesc :: String, spaceTopLevelPages :: [Page] }
-- data Page = Page { pageTitle :: String, pageContents :: String, pageChildren :: [Page], pageAttachments :: [Attachment], pageLabels :: [String] }
-- data Attachment = Attachment { attachmentName :: String, attachmentFilePath :: String }

encodeFilename :: String -> String
-- encodeFilename ('_':s) = 'I':encodeFilename s -- remove leading underscores from filenames
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

createPage :: String -> Map (String, String) String -> Map String String -> [String] -> Page -> IO ()
createPage spacekey pm am spacekeys (Page title contents children attachments labels) = do
  let title' = encodeFilename title
  case (title', children) of
    ("", _) -> return ()
    (_, []) -> convertAndWritePage title title'
    _       -> do
      convertAndWritePage title title'
      createDirectoryIfMissing False title'
      setCurrentDirectory title'
      mapM_ (createPage spacekey pm am spacekeys) children
      setCurrentDirectory ".."
  where
    contents' = confluenceToPandoc spacekey title pm am spacekeys contents
    convertAndWritePage title'' title''' = do
      mapM_ (\(Attachment name path) -> copyAttachment path name) attachments
      let filename = title''' ++ ".page"
      removeFile filename `catch` ignoreError
      -- writeFile filename $ (if title'' == title''' then "" else "---\ntitle: " ++ title'' ++ "\n...\n\n") ++ contents'
      writeFile filename $ "---\ntitle: " ++ title'' ++ labelText labels ++ "\n...\n\n" ++ contents'
    ignoreError :: IOException -> IO ()
    ignoreError _ = return ()
    labelText [] = ""
    labelText ls = "\nkeywords: [" ++ intercalate ", " ls ++ "]"

createPageMap :: [(String, Page)] -> Map (String, String) String
createPageMap = createPageMap' "/" empty
  where
    createPageMap' _ pm [] = pm
    createPageMap' path pm ((spacekey, Page title _ children _ _):ps) = let title' = encodeFilename title
                                                                            path' = path </> title'
                                                                        in  createPageMap' path (createPageMap' path' (insert (spacekey, title') path' pm) $ map (\p -> (spacekey, p)) children) ps

createAnchorMap :: [Page] -> Map String String
createAnchorMap ps = fromList $ map makeMapping $ concatMap createAnchors ps
  where
    createAnchors (Page _ contents children _ _) = getHeaders contents ++ concatMap createAnchors children
    makeMapping name = (filter (not . isSpace) name, toIdent name)

getData :: [String] -> IO ()
getData spacekeys = do
  conn <- connect defaultConnectInfo {ciUser = "confluence", ciPassword = confPass, ciDatabase = "confluence"}
  spaces <- mapMaybeM (getSpace conn) spacekeys
  let pm = createPageMap $ concatMap (\s -> map (\p -> (spaceKey s, p)) $ spaceTopLevelPages s) spaces
  let am = createAnchorMap $ concatMap spaceTopLevelPages spaces
  forM_ spaces $ \(Space spacekey title _ tlpages) -> do
    createDirectoryIfMissing False title
    setCurrentDirectory title
    forM_ tlpages (createPage spacekey pm am spacekeys)
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
