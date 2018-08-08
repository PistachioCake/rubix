{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}

module Rubix (
    Color, Side, Cube,
    solved, apply, 
    turnF, turnU, turnR, turnD, turnL, turnB, 
    unsafeMkCube
)where

import Control.Lens

data Vec3 a = Vec3 a a a deriving (Functor, Eq)

data Side a = Side {
  _tl :: a,
  _tc :: a,
  _tr :: a,
  _ml :: a,
  _mc :: a,
  _mr :: a,
  _bl :: a,
  _bc :: a,
  _br :: a} deriving (Show, Eq)

top, middle, bottom :: Lens' (Side a) (Vec3 a)
top    = lens (\(Side tl tc tr _ _ _ _ _ _) -> Vec3 tl tc tr) (\(Side _ _ _ ml mc mr bl bc br) (Vec3 tl tc tr) -> Side tl tc tr ml mc mr bl bc br)
middle = lens (\(Side _ _ _ ml mc mr _ _ _) -> Vec3 ml mc mr) (\(Side tl tc tr _ _ _ bl bc br) (Vec3 ml mc mr) -> Side tl tc tr ml mc mr bl bc br)
bottom = lens (\(Side _ _ _ _ _ _ bl bc br) -> Vec3 bl bc br) (\(Side tl tc tr ml mc mr _ _ _) (Vec3 bl bc br) -> Side tl tc tr ml mc mr bl bc br)

left, center, right :: Lens' (Side a) (Vec3 a)
left   = lens (\(Side tl _ _ ml _ _ bl _ _) -> Vec3 tl ml bl) (\(Side _ tc tr _ mc mr _ bc br) (Vec3 tl ml bl) -> Side tl tc tr ml mc mr bl bc br)
center = lens (\(Side _ tc _ _ mc _ _ bc _) -> Vec3 tc mc bc) (\(Side tl _ tr ml _ mr bl _ br) (Vec3 tc mc bc) -> Side tl tc tr ml mc mr bl bc br)
right  = lens (\(Side _ _ tr _ _ mr _ _ br) -> Vec3 tr mr br) (\(Side tl tc _ ml mc _ bl bc _) (Vec3 tr mr br) -> Side tl tc tr ml mc mr bl bc br)

rotCW, rotCCW :: Side a -> Side a
rotCW  (Side tl tc tr ml mc mr bl bc br) = Side bl ml tl bc mc tc br mr tr
rotCCW (Side tl tc tr ml mc mr bl bc br) = Side tr mr br tc mc bc tl ml bl

data Color = White | Red | Blue | Orange | Green | Yellow deriving (Show, Eq)

type Face = Side Color

-- | A model of a rubix cube
data Cube = Cube {
  _front :: Face,
  _up    :: Face,
  _right :: Face,
  _down  :: Face,
  _left  :: Face,
  _back  :: Face
} deriving (Show, Eq)

solved :: Cube
solved = Cube (Side White  White  White  White  White  White  White  White  White )
              (Side Blue   Blue   Blue   Blue   Blue   Blue   Blue   Blue   Blue  )
              (Side Red    Red    Red    Red    Red    Red    Red    Red    Red   )
              (Side Green  Green  Green  Green  Green  Green  Green  Green  Green )
              (Side Orange Orange Orange Orange Orange Orange Orange Orange Orange)
              (Side Yellow Yellow Yellow Yellow Yellow Yellow Yellow Yellow Yellow)

turnF :: Cube -> Cube
turnF (Cube f u r d l b) = Cube 
    (rotCW f)
    (set bottom (view right l) u) 
    (set left (view bottom u) r) 
    (set top (view left r) d) 
    (set right (view top d) l) 
    b

turnU :: Cube -> Cube
turnU (Cube f u r d l b) = Cube 
    (set top (view top r) f)
    (rotCW u)
    (set top (view top b) r)
    d
    (set top (view top f) l)
    (set top (view top l) b)

turnR :: Cube -> Cube
turnR (Cube f u r d l b) = Cube 
    (set right (view right d) f)
    (set right (view right f) u)
    (rotCW r)
    (set right (view left b) d)
    l
    (set left (view right u) b)

turnD :: Cube -> Cube
turnD (Cube f u r d l b) = Cube 
    (set bottom (view bottom l) f)
    u
    (set bottom (view bottom f) r)
    (rotCW d)
    (set bottom (view bottom b) l)
    (set bottom (view bottom r) b)

turnL :: Cube -> Cube
turnL (Cube f u r d l b) = Cube
    (set left (view left u) f)
    (set left (view right b) u)
    r
    (set left (view left f) d)
    (rotCW l)
    (set right (view left d) b)

turnB :: Cube -> Cube
turnB (Cube f u r d l b) = Cube
    f
    (set top (view right r) u)
    (set right (view bottom d) r)
    (set bottom (view left l) d)
    (set left (view top u) l)
    (rotCW b)

apply :: Cube -> (Cube -> Cube) -> Cube
apply = flip ($)
infixl 7 `apply`
