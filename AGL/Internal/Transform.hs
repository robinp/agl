module AGL.Internal.Transform 
  ( execTransform
  , TranslateDrawState
  , translatePivot
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
-- >>> import Data.Default (def)

-- Transformation of DrawState -----
type TranslateDrawState = Vector -> DrawState -> DrawState

translate :: TranslateDrawState
translate (x, y) = dsX +~ x >>> dsY +~ y

mulAlpha a = dsAlpha %~ mappend a

-- | Folds transforms along paths of the @Picture@ tree, 
--   and traverses the resulting leaves using the supplied executor function.
--
-- The type of the leaf images is not considered, any further translation
-- (for example taking an image pivot into account) should be performed
-- by the executor function. For this it should use the supplied @TranslateDrawState@
-- function, which applied the translation on top of the current transform stack.
-- 
-- >>> let translateAndEchoDS _ ds trans = pure $ trans (-1.0, 2.0) ds
-- >>> let ds' = head $ runIdentity $ execTransform def (Draw "dummy") translateAndEchoDS
-- >>> (ds'^.dsX, ds'^.dsY)
-- (-1.0,2.0)
--
-- >>> let dummy _ ds trans = pure ()
-- >>> length $ runIdentity $ execTransform def (Pictures [Draw "apple", Draw "pie"]) dummy
-- 2
--
-- >>> let echoDS _ ds _ = pure $ (ds^.dsX, ds^.dsY)
-- >>> let subPicA = Translated (0,1) (Draw "a")
-- >>> let subPicB = Translated (0,2) (Draw "b")
-- >>> runIdentity $ execTransform def (Translated (-1,0) $ Pictures [subPicA, subPicB]) echoDS
-- [(-1.0,1.0),(-1.0,2.0)]
--
execTransform :: (Applicative f) => DrawState -> Picture a -> (a -> DrawState -> TranslateDrawState -> f b) -> f [b]
execTransform ds pic exec = execTransform' ds pic
  where
  execTransform' ds pic = case pic of
    Pictures ps     -> concat <$> traverse (execTransform' ds) ps
    Translated v p  -> execTransform' (translate v ds) p 
    Draw a          -> sequenceA [exec a ds translate]
    Transparent a p -> execTransform' (mulAlpha a ds) p

-- | Translates the image so that when drawn, the pivot point falls on the original position.
--   Provided for convenience, can be used by implementing backends.
translatePivot :: (Image a) => TranslateDrawState -> a -> DrawState -> DrawState
translatePivot trans a = trans (both %~ negate $ imagePivotXY a)
