module AGL.SDL 
  ( SurfImage
  , fromSDLSurface
  , renderSDL
  ) where

import Control.Lens
import Control.Monad (void)

import Data.Default (def)

import qualified Graphics.UI.SDL.Types as SDLT
import qualified Graphics.UI.SDL.Video as SDLV
import qualified Graphics.UI.SDL.Rect as SDLR

import AGL
import AGL.Internal.Transform
import AGL.Internal.Transform.DrawState

data SurfImage = SurfImage {
  imSurface :: SDLT.Surface,
  imW :: Double,
  imH :: Double,
  imPivot :: Pivot}

instance Image SurfImage where
  imageH = imH
  imageW = imW
  imagePivot = imPivot

-- TODO add fromSDLSubSurface
fromSDLSurface s p = SurfImage {
  imSurface = s,
  imW = fromIntegral $ SDLT.surfaceGetWidth s,
  imH = fromIntegral $ SDLT.surfaceGetHeight s,
  imPivot = p}

-- | Draws the image to the SDL surface.
--
-- Blitting errors are ignored for now (are they even possible? With which SDL backends?)
-- 
drawSurfImage :: SDLT.Surface -> SurfImage -> DrawState -> TranslateDrawState -> IO ()
drawSurfImage target si ds translate = 
  let srcW = (SDLT.surfaceGetWidth . imSurface) si 
      srcH = (SDLT.surfaceGetHeight . imSurface) si
      targetH = SDLT.surfaceGetHeight target
      ds' = translatePivot translate si ds
      aglBottomLeftX = ds'^.dsX
      aglBottomLeftY = ds'^.dsY
      sdlTopLeftX = round aglBottomLeftX
      sdlTopLeftY = targetH - round aglBottomLeftY - srcH
      drect = SDLR.Rect sdlTopLeftX sdlTopLeftY srcW srcH
      srcSurf = imSurface si
      alphaByte = floor . (255 *) . min 1.0 . max 0.0 . unAlpha
  in do
    SDLV.setAlpha srcSurf [SDLT.SrcAlpha] (ds^.dsAlpha.to alphaByte)
    SDLV.blitSurface srcSurf Nothing target (Just drect)
    return ()

-- | Draws the picture by pushing transforms through the standard AGL Transform
-- | and delegating execution to drawSurfImage.
drawSDL ds target pic = void $ execTransform ds pic (drawSurfImage target)

-- | Draws the picture and flips the screen surface.
renderSDL :: SDLT.Surface -> Picture SurfImage -> IO ()
renderSDL screen pic = drawSDL def screen pic >> SDLV.flip screen

