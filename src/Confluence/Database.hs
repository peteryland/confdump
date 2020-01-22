{-# LANGUAGE OverloadedStrings #-}

module Confluence.Database
    ( getSpace,
      Space(..),
      Page(..),
      Attachment(..),
    ) where

import System.FilePath((</>))
import Data.Maybe(listToMaybe, fromMaybe)
import Data.Monoid(mempty)
import qualified Data.SortedList as SL
import qualified Data.Text as T
import Database.MySQL.Base
import qualified System.IO.Streams as S
import qualified Data.IntMap as IM

data Space = Space { spaceName :: String, spaceDesc :: String, spaceTopLevelPages :: [Page] }
data Page = Page { pageTitle :: String, pageContents :: String, pageChildren :: [Page], pageAttachments :: [Attachment] }
data Attachment = Attachment { attachmentName :: String, attachmentFilePath :: String }

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

instance Show Space where
  show space = unlines [ "Name: " ++ spaceName space,
                         "Description: " ++ spaceDesc space,
                         "Homepage: " ++ (fromMaybe "(No Homepage)" $ pageTitle <$> (listToMaybe $ spaceTopLevelPages space))
                       ]

instance Show Page where
  show page = unlines [ "Title: " ++ pageTitle page,
                        "Contents:",
                        indent 2 $ pageContents page
                      ]

data DBContentType = DBCTPage | DBCTAttachment | DBCTSpaceDescription deriving (Show, Eq, Ord)
data DBContent = DBContent { dbContentChildPositionOrVersion :: Int, dbContentId :: Int, dbContentType :: DBContentType, dbContentTitle :: String } deriving (Show, Eq, Ord)
data DBSpace = DBSpace { dbSpaceId :: Int, dbSpaceName :: String, dbSpaceDescId :: Int, dbHomepage :: Int } deriving Show

-- from Control.Monad.Extra:
-- | A version of 'mapMaybe' that works with a monadic predicate.
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
{-# INLINE mapMaybeM #-}
mapMaybeM op = foldr f (return [])
  where f x xs = do x' <- op x; case x' of Nothing -> xs; Just x'' -> do xs' <- xs; return $ x'':xs'

queryBodyContent :: MySQLConn -> Int -> IO String
queryBodyContent conn pageId = do
  s <- prepareStmt conn "SELECT BODY FROM BODYCONTENT WHERE CONTENTID=?"
  (_, bodycontent) <- queryStmt conn s [MySQLInt64 $ fromIntegral pageId]
  body <- listToMaybe <$> S.toList bodycontent
  return $ unwrapMySQLText $ (listToMaybe =<< body)
  where
    unwrapMySQLText (Just (MySQLText t)) = T.unpack t
    unwrapMySQLText _ = ""

getAttachment :: Int -> Int -> DBContent -> IO (Maybe Attachment)
getAttachment spaceid pageid (DBContent version contentid DBCTAttachment name) = return $ Just $ Attachment name filepath
  where
    filepath = "/var/lib/confluence/attachments/ver003" </> show (spaceid `mod` 250) </> show (spaceid `div` 1000 `mod` 250) </> show spaceid </>
                                                            show (pageid  `mod` 250) </> show (pageid  `div` 1000 `mod` 250) </> show pageid  </>
                                                            show contentid </> show version
getAttachment _ _ _ = return Nothing

getPage :: MySQLConn -> Int -> IM.IntMap (SL.SortedList DBContent) -> IM.IntMap [DBContent] -> DBContent -> IO (Maybe Page)
getPage conn spaceid pagemap attachmentmap (DBContent _ pageid DBCTPage title) = do
  bodycontent <- queryBodyContent conn pageid
  children <- mapMaybeM (getPage conn spaceid pagemap attachmentmap) $ maybe [] id $ SL.fromSortedList <$> IM.lookup pageid pagemap
  attachments <- mapMaybeM (getAttachment spaceid pageid) $ maybe [] id $ IM.lookup pageid attachmentmap
  return $ Just $ Page title bodycontent children attachments
getPage _ _ _ _ _ = return Nothing

getSpace :: MySQLConn -> String -> IO (Maybe Space)
getSpace conn spacekey = do
  space <- querySpace conn spacekey
  case space of
    Nothing -> return Nothing
    Just (DBSpace spaceid name descid _) -> do
      desc <- queryBodyContent conn descid
      (tlpages, pagemap, attachmentmap) <- queryContent conn spaceid
      tlpages' <- mapMaybeM (getPage conn spaceid pagemap attachmentmap) (SL.fromSortedList tlpages)
      return $ Just $ Space name desc tlpages'

querySpace :: MySQLConn -> String -> IO (Maybe DBSpace)
querySpace conn spacekey = do
  s <- prepareStmt conn "SELECT SPACEID, SPACENAME, SPACEDESCID, HOMEPAGE FROM SPACES WHERE SPACEKEY=?"
  (_, is) <- queryStmt conn s [MySQLText $ T.pack spacekey]
  space <- unpackDBSpace <$> S.toList is
  return space
  where
    unpackDBSpace [[MySQLInt64 spaceid, MySQLText name, MySQLInt64 descid, MySQLInt64 homepage]] = Just $ DBSpace (fromIntegral spaceid) (T.unpack name) (fromIntegral descid) (fromIntegral homepage)
    unpackDBSpace _ = Nothing

queryContent :: MySQLConn -> Int -> IO (SL.SortedList DBContent, IM.IntMap (SL.SortedList DBContent), IM.IntMap [DBContent])
queryContent conn spaceid = do
  s1 <- prepareStmt conn "SELECT CONTENTID, CONTENTTYPE, TITLE, PARENTID, CHILD_POSITION, PAGEID, VERSION FROM CONTENT WHERE SPACEID=? AND CONTENTTYPE<>'CUSTOM' AND CONTENT_STATUS='current'"
  (_, is) <- queryStmt conn s1 [MySQLInt64 $ fromIntegral spaceid]
  unpackDBContent <$> S.toList is
  where
    unpackDBContent [] = (mempty, IM.empty, IM.empty)
    unpackDBContent ([MySQLInt64 _, MySQLText "SPACEDESCRIPTION", MySQLNull, MySQLNull, MySQLNull, MySQLNull, MySQLInt32 _]:cs) = unpackDBContent cs
    unpackDBContent ([MySQLInt64 contentid, MySQLText ctype, MySQLText title, parentid, childpos, pageid, MySQLInt32 version]:cs)
                      = let contentid' = fromIntegral contentid
                            ctype' = dbContentTypeFromText ctype
                            title' = T.unpack title
                            (tlpages, pagemap, attachmentmap) = unpackDBContent cs
                        in  (case (ctype', parentid, childpos, pageid) of
                              (Just DBCTPage,       MySQLNull,            MySQLInt32 childpos', MySQLNull)          -> (SL.insert (DBContent (fromIntegral childpos') contentid' DBCTPage title') tlpages, pagemap, attachmentmap)
                              (Just DBCTPage,       MySQLInt64 parentid', MySQLInt32 childpos', MySQLNull)          -> (tlpages, alter' (DBContent (fromIntegral childpos') contentid' DBCTPage title') (fromIntegral parentid') pagemap, attachmentmap)
                              (Just DBCTPage,       MySQLInt64 parentid', MySQLNull           , MySQLNull)          -> (tlpages, alter' (DBContent 0                        contentid' DBCTPage title') (fromIntegral parentid') pagemap, attachmentmap)
                              (Just DBCTAttachment, MySQLNull,            MySQLNull,            MySQLInt64 pageid') -> (tlpages, pagemap, alter'' (DBContent (fromIntegral version) contentid' DBCTAttachment title') (fromIntegral pageid') attachmentmap)
                              _                                                                                     -> error $ "Unknown content type in space" ++ show [MySQLInt64 contentid, MySQLText ctype, MySQLText title, parentid, childpos, pageid, MySQLInt32 version]
                            )
    unpackDBContent (t:_) = error $ "Unknown content type in space: " ++ show t
    alter'  val = IM.alter (Just . maybe mempty (SL.insert val))
    alter'' val = IM.alter (Just . maybe mempty (val:))
    dbContentTypeFromText t = case T.unpack t of
      "PAGE"             -> Just DBCTPage
      "ATTACHMENT"       -> Just DBCTAttachment
      "SPACEDESCRIPTION" -> Just DBCTSpaceDescription
      _                  -> Nothing
