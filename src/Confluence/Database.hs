{-# LANGUAGE OverloadedStrings #-}

module Confluence.Database
    ( getSpace,
      Space(..),
      Page(..),
    ) where

import Data.Maybe(listToMaybe, mapMaybe)
import Data.List(sortOn)
import qualified Data.Text as T
import Database.MySQL.Base
import qualified System.IO.Streams as S

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

unwrapMySQLText :: MySQLValue -> String
unwrapMySQLText (MySQLText t) = T.unpack t
unwrapMySQLText _ = ""

unwrapMySQLInt :: MySQLValue -> Maybe Int
unwrapMySQLInt (MySQLInt64 t) = Just $ fromIntegral t
unwrapMySQLInt (MySQLInt32 t) = Just $ fromIntegral t
unwrapMySQLInt (MySQLInt8U t) = Just $ fromIntegral t
unwrapMySQLInt (MySQLInt8 t) = Just $ fromIntegral t
unwrapMySQLInt (MySQLInt16U t) = Just $ fromIntegral t
unwrapMySQLInt (MySQLInt16 t) = Just $ fromIntegral t
unwrapMySQLInt (MySQLInt32U t) = Just $ fromIntegral t
unwrapMySQLInt (MySQLInt64U t) = Just $ fromIntegral t
unwrapMySQLInt (MySQLYear t) = Just $ fromIntegral t
unwrapMySQLInt _ = Nothing

getOne :: [a] -> Maybe a
getOne = listToMaybe

getTwo :: [a] -> Maybe (a, a)
getTwo (x:y:_) = Just (x, y)
getTwo _ = Nothing

getThree :: [a] -> Maybe (a, a, a)
getThree (x:y:z:_) = Just (x, y, z)
getThree _ = Nothing

-- getFour :: [a] -> Maybe (a, a, a, a)
-- getFour (w:x:y:z:_) = Just (w, x, y, z)
-- getFour _ = Nothing

maybeToString :: Maybe String -> String
maybeToString (Just s) = s
maybeToString Nothing = ""

getBodyContent :: MySQLConn -> MySQLValue -> IO String
getBodyContent conn pageId = do
  s <- prepareStmt conn "SELECT BODY FROM BODYCONTENT WHERE CONTENTID=?"
  (_, bodycontent) <- queryStmt conn s [pageId]
  body <- getOne <$> S.toList bodycontent
  return $ maybeToString $ unwrapMySQLText <$> (getOne =<< body)

getPage :: MySQLConn -> MySQLValue -> IO Page
getPage conn pageId = do
  s1 <- prepareStmt conn "SELECT TITLE FROM CONTENT WHERE CONTENTID=?"
  (_, content) <- queryStmt conn s1 [pageId]
  content' <- getOne <$> S.toList content
  let title = maybeToString $ unwrapMySQLText <$> (getOne =<< content')

  bodycontent <- getBodyContent conn pageId

  s2 <- prepareStmt conn "SELECT CONTENTID, CHILD_POSITION FROM CONTENT WHERE PARENTID=?"
  (_, children) <- queryStmt conn s2 [pageId]
  children' <- S.toList children
  children'' <- mapM (getPage conn . fst) $ sortOn (unwrapMySQLInt . snd) $ mapMaybe getTwo children'

  return $ Page title bodycontent children''

  -- Attachments:
  -- s3 <- prepareStmt conn "SELECT CONTENTID, TITLE, SPACEID, HIBERNATEVERSION FROM CONTENT WHERE CONTENTTYPE='ATTACHMENT' AND CONTENTSTATUS='current' AND PREVVER=NULL AND PAGEID=?"
  -- (_, attachments) <- queryStmt conn s3 [pageId]
  -- attachments' <- getFour <$> S.toList attachments
  -- -- filename on filesystem is "/var/lib/confluence/attachments/ver003/" ++ show (spaceId `mod` 250) ++ "/" ++ show (spaceId `div` 1000 `mod` 250) ++ "/" ++ show spaceId ++ "/" ++
  --                                                                           show (pageId  `mod` 250) ++ "/" ++ show (pageId  `div` 1000 `mod` 250) ++ "/" ++ show pageId  ++ "/" ++
  --                                                                           show contentId ++ "/" ++ show hibernateVersion

  -- Content:
  -- title, parentid, child_position

  -- s2 <- prepareStmt conn "SELECT * FROM CONTENTPROPERTIES WHERE CONTENTID=?"
  -- (_, contentproperties) <- queryStmt conn s2 [pageId]
  -- -- ContentProperties:
  -- -- propertyid, propertyname, stringval, longval, dateval, contentid
  -- print =<< S.toList contentproperties

  -- s4 <- prepareStmt conn "SELECT * FROM CONFANCESTORS where ANCESTORID=?"
  -- (_, descendents) <- queryStmt conn s4 [pageId]
  -- -- ConfAncestors:
  -- -- descendentid, ancestorid, ancestorposition
  -- print =<< S.toList descendents

  -- s5 <- prepareStmt conn "SELECT * FROM CONFANCESTORS where DESCENDENTID=?"
  -- (_, ancestors) <- queryStmt conn s5 [pageId]
  -- -- ConfAncestors:
  -- -- descendentid, ancestorid, ancestorposition
  -- print =<< S.toList ancestors

data Space = Space { spaceName :: String, spaceDesc :: String, spaceHomePage :: Page }
data Page = Page { pageTitle :: String, pageContents :: String, pageChildren :: [Page] }

instance Show Space where
  show space = unlines [ "Name: " ++ spaceName space,
                         "Description: " ++ spaceDesc space,
                         "Homepage:",
                         indent 2 $ show $ spaceHomePage space
                       ]

instance Show Page where
  show page = unlines [ "Title: " ++ pageTitle page,
                        "Contents:",
                        indent 2 $ pageContents page,
                        "Children:"
                      ]
              ++ showChildren page
    where
      showChildren pg = case pageChildren pg of
        [] -> ""
        children -> indent 2 $ concatMap show' children
      show' p = pageTitle p ++ "\n" ++ showChildren p

getSpace :: MySQLConn -> String -> IO (Maybe Space)
getSpace conn spacekey = do
  s <- prepareStmt conn "SELECT SPACENAME, SPACEDESCID, HOMEPAGE FROM SPACES WHERE SPACEKEY=?"
  (_, is) <- queryStmt conn s [MySQLText $ T.pack spacekey]
  space <- getOne <$> S.toList is
  mapM (\(spacename, spacedescid, homepageid) -> do
        spacedesc <- getBodyContent conn spacedescid
        homepage <- getPage conn homepageid

        -- s2 <- prepareStmt conn "SELECT * FROM CONFANCESTORS where ANCESTORID=?"
        -- (_, descendents) <- queryStmt conn s2 [homepage]
        -- -- ConfAncestors:
        -- -- descendentid, ancestorid, ancestorposition
        -- descendents' <- S.toList descendents
        -- mapM_ (getPage conn) $ homepage : map head descendents'

        return $ Space (unwrapMySQLText spacename) spacedesc homepage
       ) (getThree =<< space)
