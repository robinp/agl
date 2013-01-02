module AGL.SDL 
  ( SurfImage
  , fromSDLSurface
  , renderSDL
  ) where

import Control.Lens

import Data.Default (def)

import qualified Graphics.UI.SDL.Types as SDLT
import qualified Graphics.UI.SDL.Video as SDLV
import qualified Graphics.UI.SDL.Rect as SDLR

import AGL
import AGL.Internal.Transform
import AGL.Internal.Transform.DrawState

data SurfImage = SurfImage {
  _imSurface :: SDLT.Surface,
  _imW :: Double,
  _imH :: Double,
  _imPivot :: Pivot}

instance Image SurfImage where
  imageH = _imH
  imageW = _imW
  imagePivot = _imPivot

-- TODO add fromSDLSubSurface
fromSDLSurface s p = SurfImage {
  _imSurface = s,
  _imW = fromIntegral $ SDLT.surfaceGetWidth s,
  _imH = fromIntegral $ SDLT.surfaceGetHeight s,
  _imPivot = p}

-- | Draws the image to the SDL surface.
--
-- Blitting errors are ignored for now (are they even possible? With which SDL backends?)
-- 
drawSurfImage :: SDLT.Surface -> SurfImage -> DrawState -> IO ()
drawSurfImage target si ds = 
  let srcW = (SDLT.surfaceGetWidth . _imSurface) si 
      srcH = (SDLT.surfaceGetHeight . _imSurface) si
      targetH = SDLT.surfaceGetHeight target
      aglBottomLeftX = ds^.dsX
      aglBottomLeftY = ds^.dsY
      sdlTopLeftX = round aglBottomLeftX
      sdlTopLeftY = targetH - (round aglBottomLeftY) - srcH
      drect = SDLR.Rect sdlTopLeftX sdlTopLeftY srcW srcH
      srcSurf = _imSurface si
      alphaByte = floor . (255 *) . min 1.0 . max 0.0 . unAlpha
  in do
    SDLV.setAlpha srcSurf [SDLT.SrcAlpha] (ds^.dsAlpha.to alphaByte)
    SDLV.blitSurface (_imSurface si) Nothing target (Just drect)
    return ()

-- | Draws the picture by pushing transforms through the standard AGL Transform
-- | and delegating execution to drawSurfImage.
drawSDL ds target pic = execTransform ds target pic drawSurfImage >> return ()

-- | Draws the picture and flips the screen surface.
renderSDL :: SDLT.Surface -> Picture SurfImage -> IO ()
renderSDL screen pic = drawSDL def screen pic >> SDLV.flip screen

