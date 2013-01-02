{-# LANGUAGE TemplateHaskell #-}

module AGL.Internal.Transform.DrawState
  ( DrawState(..)
  -- * Lenses
  , dsX, dsY, dsAlpha
  ) where

import Control.Lens

import Data.Default
import Data.Monoid

import AGL

data DrawState = DrawState {
  _dsX :: Double,
  _dsY :: Double,
  _dsAlpha :: Alpha} deriving Eq

makeLenses ''DrawState

-- | Defines an identity transform as default.
instance Default DrawState where
  def = DrawState 0 0 mempty


