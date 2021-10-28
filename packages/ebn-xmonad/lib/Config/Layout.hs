{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Config.Layout where
import XMonad
import qualified XMonad.StackSet as W
import GHC.Base (ap)
import Data.Ratio
import Control.Monad
import XMonad.Prompt.Window (WindowPrompt(Goto))

data CenterWork a = CenterWork
  { threeColNMaster' :: !Int,
    threeColDelta' :: !Rational,
    threeColFrac' :: !Rational
  }
  deriving (Read, Show)

instance LayoutClass CenterWork a where
  runLayout = applyPos
  description _ = "CenterWork"
--  handleMessage m = msum [ fmap f (fromMessage m) ]
--    where f (Goto w) = _

applyPos :: (LayoutClass l a) => W.Workspace WorkspaceId (l a) a -> Rectangle -> X ([(a, Rectangle)], Maybe (l a))
applyPos (W.Workspace _ l s) rect = do
  case (ws, s) of
    ([w], Just s) -> pure ([(w, centerM (1 / 2) 1 rect)], Nothing)
    ([w1,w2], Just s) -> pure (doL True 1 (3/7) rect s, Nothing)
    (w : ws, Just s) -> pure (doL True 1 (3 / 7) rect s, Nothing)
    _ -> emptyLayout l rect
  where
    ws = W.integrate' s

centerM :: Float -> Float -> Rectangle -> Rectangle
centerM rx ry (Rectangle sx sy sw sh) = Rectangle x y w h
  where
    w = round (fromIntegral sw * rx)
    h = round (fromIntegral sh * ry)
    x = sx + fromIntegral (sw - w) `div` 2
    y = sy + fromIntegral (sh - h) `div` 2

tile2 :: Rational -> Rectangle -> (Rectangle, Rectangle)
tile2 ratio (Rectangle sx sy sw sh)  =
  (
    Rectangle (sx + fromIntegral r2w) sy r1w sh,
    Rectangle sx sy r2w sh
  )
  where
    r2w = ceiling $ (sw - r1w) % 2
    r1w = ceiling $ fromIntegral sw*ratio
  

doL :: Bool -> Int -> Rational -> Rectangle -> W.Stack a -> [(a, Rectangle)]
doL m n f r = ap zip (tile3 m f r n . length) . W.integrate

-- | tile3.  Compute window positions using 3 panes
tile3 :: Bool -> Rational -> Rectangle -> Int -> Int -> [Rectangle]
tile3 middle f r nmaster n
  | n <= nmaster || nmaster == 0 = splitVertically n r
  | n <= nmaster + 1 = splitVertically nmaster s1 ++ splitVertically (n - nmaster) s2
  | otherwise = splitVertically nmaster r1 ++ splitVertically nslave1 r3 ++ splitVertically nslave2 r2
  where
    (r1, r2, r3) = split3HorizontallyBy middle (if f < 0 then 1 + 2 * f else f) r
    (s1, s2) = tile2 (if f < 0 then 1 +2 * f else f) r
      --splitHorizontallyBy (if f < 0 then 1 + f else f) r
    nslave = n - nmaster
    nslave1 = ceiling (nslave % 2)
    nslave2 = n - nmaster - nslave1

split3HorizontallyBy :: Bool -> Rational -> Rectangle -> (Rectangle, Rectangle, Rectangle)
split3HorizontallyBy middle f (Rectangle sx sy sw sh) =
  if middle
    then
      ( Rectangle (sx + fromIntegral r3w) sy r1w sh,
        Rectangle sx sy r3w sh,
        Rectangle (sx + fromIntegral r3w + fromIntegral r1w) sy r2w sh
      )
    else
      ( Rectangle sx sy r1w sh,
        Rectangle (sx + fromIntegral r1w) sy r2w sh,
        Rectangle (sx + fromIntegral r1w + fromIntegral r2w) sy r3w sh
      )
  where
    r1w = ceiling $ fromIntegral sw * f
    r2w = ceiling ((sw - r1w) % 2)
    r3w = sw - r1w - r2w

    --else
    --  ( centerM (1/2) 1 (Rectangle sx sy sw sh),
    --    Rectangle (sx + fromIntegral r1w) sy r2w sh,
    --    Rectangle (sx + fromIntegral r1w + fromIntegral r2w) sy r3w sh
    --  )

--pickLayout :: W.Workspace WorkspaceId (l a) a -> Rectangle -> [Rectangle]
--pickLayout wsp rect = case windows of
--  []       -> pure rect
--  [w]      -> pure (centerM (1 / 2) 1 rect)
--  (w:ws)   -> tile3 True (3 / 7) rect 1 (length ws)
--  where
--    windows = W.integrate' $ W.stack wsp
--    layout = W.layout wsp
