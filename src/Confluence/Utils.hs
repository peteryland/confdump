module Confluence.Utils(safeHead, mapMaybeM, (&&&), toIdent) where

import Data.Char(isAlpha, isAlphaNum, toLower, isSpace)

safeHead :: [a] -> Maybe a
safeHead (a:_) = Just a
safeHead _ = Nothing

-- directly from Control.Monad.Extra:
-- | A version of 'mapMaybe' that works with a monadic predicate.
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
{-# INLINE mapMaybeM #-}
mapMaybeM op = foldr f (return [])
  where f x xs = do x' <- op x; case x' of Nothing -> xs; Just x'' -> do xs' <- xs; return $ x'':xs'

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(f &&& g) a = (f a, g a)

toIdent :: String -> String
toIdent = dropWhile (not . isAlpha) . toIdent'
  where
    toIdent' (c:cs) | isSpace c = '-':toIdent' (dropWhile isSpace cs)
    toIdent' (c:cs) | c == '-' || c == '_' || c == '.'  || isAlphaNum c = toLower c:toIdent' cs
    toIdent' (_:cs) = toIdent' cs
    toIdent' "" = ""
