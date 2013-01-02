module Main (main) where

import qualified Graphics.UI.SDL.General as SG
import qualified Graphics.UI.SDL.Types as ST
import qualified Graphics.UI.SDL.Time  as STime
import qualified Graphics.UI.SDL.Video as SV

import Control.Applicative
import Control.Monad.Reader

import AGL
import AGL.SDL

main = do
  SG.init [SG.InitEverything]
  putStrLn "Loading resources"
  img <- fromSDLSurface <$> SV.loadBMP "cow.bmp" <*> pure pivotCenter
  putStrLn "Initing GFX"
  surf <- SV.trySetVideoMode 640 480 32 [ST.HWSurface]
  runOrReport (doRender img) surf
  SG.quit

doRender img surf = do
  renderSDL surf $ Translated (100, 0) $ Transparent (alpha 0.25) $ Draw img
  STime.delay 1000

runOrReport _ Nothing  = SG.getError >>= putStrLn . ("Failed with error: " ++) . show
runOrReport f (Just x) = f x 
