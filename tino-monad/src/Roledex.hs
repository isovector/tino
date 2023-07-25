{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Roledex
-- Description :  A completely pointless layout which acts like Microsoft's Flip 3D.
-- Copyright   :  (num_wins) tim.thelion@gmail.com
-- License     :  BSD
--
-- Maintainer  :  tim.thelion@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- This is a completely pointless layout which acts like Microsoft's Flip 3D
-----------------------------------------------------------------------------

module Roledex (
    -- * Usage
    -- $usage

    -- * Screenshots
    -- $screenshot
    Roledex(Roledex), shouldFollow) where

import Data.List
import Data.Bool (bool)
import XMonad
import XMonad.Core
import qualified XMonad.StackSet as W
import Data.Ratio

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Roledex
--
-- Then edit your @layoutHook@ by adding the Roledex layout:
--
-- > myLayout =  Roledex ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see
-- <https://xmonad.org/TUTORIAL.html#customizing-xmonad the tutorial> and
-- "XMonad.Doc.Extending#Editing_the_layout_hook".

-- $screenshot
-- <<http://www.timthelion.com/rolodex.png>>

data Roledex a = Roledex deriving ( Show, Read )

instance LayoutClass Roledex Window where
    doLayout _ = roledexLayout

roledexLayout :: Eq a => Rectangle -> W.Stack a -> X ([(a, Rectangle)], Maybe (Roledex a))
roledexLayout screen@(Rectangle _ _ screen_w screen_h) ws =
  pure
    ( mconcat
        [ [(W.focus ws, f count)]
        , zipWith (\w i -> (w, f i)) dns [count + 1 ..]
        , reverse $ zipWith (\w i -> (w, f i)) (reverse ups) [0..]
        ]
    , Nothing
    )
 where
   ups    = W.up ws
   count  = length ups
   dns    = W.down ws
   num_wins = count + length dns + 1
   rw = screen_w - (16 * fromIntegral num_wins)
   rh = screen_h - (16 * fromIntegral num_wins)
   -- rect {-@(Rectangle _ _ rw rh)-} = fst $ splitHorizontallyBy (7%8 :: Ratio Int) $ fst (splitVerticallyBy (7%8 :: Ratio Int) screen)
   rect = screen { rect_width = rw, rect_height = rh } -- Rectangle 0 0 rw rh
   gw = div' (screen_w - rw) (fromIntegral $ num_wins - 1)
   gh = div' (screen_h - rh) (fromIntegral $ num_wins - 1)
   mrect  mx my (Rectangle x y screen_w screen_h) = Rectangle (x + fromIntegral mx) (y + fromIntegral my) screen_w screen_h
   f n = mrect (gw * fromIntegral n) (gh * fromIntegral n) $ bool rect screen $ num_wins == 1

div' :: Integral a => a -> a -> a
div' _ 0 = 0
div' n o = div n o


isRoledex :: X Bool
isRoledex = fmap (isSuffixOf "Roledex") $ gets (description . W.layout . W.workspace . W.current . windowset)

shouldFollow :: X Bool
shouldFollow = fmap not isRoledex

