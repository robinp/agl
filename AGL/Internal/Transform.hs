module AGL.Internal.Transform 
  ( execTransform
  ) where

import Control.Applicative
import Control.Category ((>>>))
import Control.Lens

import Data.Monoid
import Data.Traversable

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
--   and traverses the resulting leaves using the supplied function.
-- 
-- >>> import Data.Default (def)
-- >>> let echo t img ds = return ds 
-- >>> let img = TestImage 10 20 pivotCenter
-- >>> let ds' = head $ runIdentity $ execTransform def img Blank echo
-- >>> (ds'^.dsX, ds'^.dsY)
-- (-5, -10)
execTransform :: (Applicative f, Image a) => DrawState -> Picture a -> (a -> DrawState -> f b) -> f [b]
execTransform ds pic exec = execTransform' ds pic
  where
  execTransform' ds pic = case pic of
    Blank           -> pure []
    Pictures ps     -> concat <$> traverse (execTransform' ds) ps
    Translate v p   -> execTransform' (translate v ds) p 
    Draw a          -> sequenceA $ [exec a $ translate (both %~ negate $ imagePivotXY a) ds]
    Transparent a p -> execTransform' (mulAlpha a ds) p

