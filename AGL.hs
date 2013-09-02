{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AGL where

import Data.Monoid

-- * Spatial structures

type Point  = (Double, Double)
type Vector = Point

-- * Color and alpha

data Color = Color {
  red   :: !Int,
  green :: !Int,
  blue  :: !Int}

newtype Alpha = Alpha (Product Double) deriving (Monoid, Eq)
alpha a = Alpha (Product a)
unAlpha (Alpha (Product a)) = a

-- * Pivoted things

data Pivot = Pivot {
  pivotX :: Double,
  pivotY :: Double}

pivotCenter = Pivot 0.5 0.5
pivotBottom = Pivot 0.5 0.0
pivotLeft   = Pivot 0.0 0.5
pivotRight  = Pivot 1.0 0.5

-- | Minimal complete definition is imageH, imageW and imagePivot
--
class Pivoted a where
  pivotedW :: a -> Double
  pivotedH :: a -> Double
  pivot :: a -> Pivot

  imagePivotX :: a -> Double
  imagePivotX a = imageW a * (pivotX . imagePivot) a

  imagePivotY :: a -> Double
  imagePivotY a = imageH a * (pivotY . imagePivot) a

  imagePivotXY :: a -> (Double, Double)
  imagePivotXY a = (imagePivotX a, imagePivotY a)

-- TODO move to Internal, expose only type + builder functions
data Picture a
  = Pictures [Picture a]
  | Translated Vector (Picture a)
  | Transparent Alpha (Picture a)
  | Draw a
  -- Ink Color (Picture a)

