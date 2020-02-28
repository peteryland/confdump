module Confluence.Utils(mapMaybeM) where

-- directly from Control.Monad.Extra:
-- | A version of 'mapMaybe' that works with a monadic predicate.
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
{-# INLINE mapMaybeM #-}
mapMaybeM op = foldr f (return [])
  where f x xs = do x' <- op x; case x' of Nothing -> xs; Just x'' -> do xs' <- xs; return $ x'':xs'
