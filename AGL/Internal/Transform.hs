module AGL.Internal.Transform 
  ( execTransform
  ) where

import Control.Category ((>>>))
import Control.Monad (liftM, join)
import Control.Lens

import Data.Default (def)
import Data.Monoid

import AGL
import AGL.Internal.Transform.DrawState

-- $setup
-- >>> import Control.Monad.Identity
-- >>> data TestImage = TestImage { tiW :: Double, tiH :: Double, pv :: Pivot }
-- >>> instance Image TestImage where imageW = tiW; imageH = tiH; imagePivot = pv

-- Transformation of DrawState -----
translate (x, y) = dsX +~ x >>> dsY +~ y

mulAlpha a = dsAlpha %~ mappend a

-- | Folds transforms along paths of the @Picture@ tree, 
--   executing the supplied monadic function at the leaves and accumulating results.
-- 
-- >>> let echo t img ds = return ds 
-- >>> let img = TestImage 10 20 pivotCenter
-- >>> let ds' = head $ runIdentity $ execTransform def img Blank echo
-- >>> (ds'^.dsX, ds'^.dsY)
-- (-5, -10)
execTransform :: (Monad m, Image a) => DrawState -> t -> Picture a -> (t -> a -> DrawState -> m b) -> m [b]
execTransform ds target pic f = execTransform' ds pic
  where
  execTransform' ds pic = case pic of
    Blank           -> return []
    Pictures ps     -> liftM join $ mapM (execTransform' ds) ps
    Translate v p   -> execTransform' (translate v ds) p 
    Draw a          -> liftM return $ f target a $ translate (both %~ negate $ imagePivotXY a) ds
    Transparent a p -> execTransform' (mulAlpha a ds) p

