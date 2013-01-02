{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AGL where

import Data.Monoid

-- * Spatial structures

type Point  = (Double, Double)
type Vector = Point

-- * Color and alpha

data Color = Color { red :: !Int, green :: !Int, blue :: !Int }

newtype Alpha = Alpha (Product Double) deriving (Monoid, Eq)
alpha a = Alpha (Product a)
unAlpha (Alpha (Product a)) = a

-- * Images

data Pivot = Pivot { pivotX :: Double, pivotY :: Double }
pivotCenter = Pivot 0.5 0.5
pivotBottom = Pivot 0.5 0.0
pivotLeft   = Pivot 0.0 0.5
pivotRight  = Pivot 1.0 0.5

-- | Minimal complete definition is imageH, imageW and imagePivot
class Image a where
  imageW :: a -> Double
  imageH :: a -> Double
  imagePivot :: a -> Pivot

  imagePivotX :: a -> Double
  imagePivotX a = imageW a * (pivotX . imagePivot) a

  imagePivotY :: a -> Double
  imagePivotY a = imageH a * (pivotY . imagePivot) a

  imagePivotXY :: a -> (Double, Double)
  imagePivotXY a = (imagePivotX a, imagePivotY a)

data Picture a
  = Blank -- TODO could be omitted (equal to Pictures []) ?
  | Pictures [Picture a]
  | Translate Vector (Picture a)
  | Transparent Alpha (Picture a)
  | Draw a
  -- Ink Color (Picture a)
  -- TODO geom stuff (rect, circle)

