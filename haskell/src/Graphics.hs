module Graphics ( bar_plot_480_48
                , Frame
                ) where

import Data (in_frame_normalize)
import Util

-- Helper functions
import Control.Monad (foldM, mplus)
import Data.Fixed (mod')

-- Static graphics library (JuicyPixels) 
import Codec.Picture


{- 
 - Bar graph graphics
 -}



-- | The fixed pixel type: 8-bit RGB pixels
type Pix = PixelRGB8

-- | The fixed image type
type Frame = Image Pix


-- | Produces a frame of the 480p plot with 48 bars
-- |    Arguments: 
-- |        ds'         a length 48 list of frequency values (*)
-- |        t           current frame of animation
-- |        num_f       total number of frames in the animation
-- |        ms'         historical frequency values (maximum of last 8 frames)
-- |
-- | (*) where are my dependent types?! why can't we do the project in Idris?!
bar_plot_480_48 :: ([Double], Int, Int, [Double]) -> Frame
bar_plot_480_48 (ds', t, num_f, ms') = generateImage f wt ht 
        where   f x y
                    -- The image is constructed in layers, from top to bottom.
                    -- because of this, the helper functions depend on each other as they can
                    -- overdraw without consequence. Mind this. 
                    | in_time_bar        = rainbow_cts (x - t)
                    | in_time_line       = PixelRGB8 200 200 200 
                    | on_grid_y          = white
                    | on_border          = white
                    | on_grid_x          = white
                    | in_vol_bar         = vol_grad (ds_nf!!(bar x))
                    | in_on_freq_bar     = bar_grad $ (fromIntegral (ht-y-32))/(fromIntegral (ht-32-56))
                    | in_hist_bar        = PixelRGB8 204 255 156
                    | mod3_shade         = PixelRGB8 220 240 220 
                    | otherwise          = white 
                                
                    where time_pct       = (fromIntegral (wt-64))*(fromIntegral t)/fromIntegral num_f
                          in_time_bar    = andmap   [ y >= ht-23
                                                    , y <= ht-21
                                                    , x > 32
                                                    , x < 32 + round time_pct]
                          in_time_line   = andmap   [ y == ht-22
                                                    , x > 32
                                                    , x < wt-32 ]
                          on_grid_y      = andmap   [ y >= 32
                                                    , y <= ht-56
                                                    , x > x_pad
                                                    , x < wt-x_pad
                                                    , (ht-56-y)`mod` 12 < 2 ]
                          on_border      = ormap    [ y > ht - 32
                                                    , y < 40
                                                    , (y>ht-56) && (y<ht-48)
                                                    , x <= x_pad
                                                    , x >= wt- x_pad ]
                          on_grid_x      = (x - x_pad + 1) `mod` wt_bar < 2
                          in_vol_bar     = y >= ht-48
                          in_on_freq_bar = y > ht-56-(ds!!(bar x))*12
                          in_hist_bar    = y >= ht-56-(ms!!(bar x))*12 
                          mod3_shade     = ((x + y) `mod` 3) == 0
                            

                -- | Constants for this animation
                wt, ht, num_bars, x_pad, levels, wt_bar :: Int
                wt              = 640   -- frame width
                ht              = 480   -- frame height
                num_bars        = 48    -- number of bars
                x_pad           = 32    -- left/right x padding
                levels          = 32    -- discrete audio levels
                wt_bar          = 12    -- width of a bar
                
                -- |Gradient for the vertical bars      
                bar_grad :: Double -> Pix
                bar_grad = pix_grad light_green green_yellow 
                
                -- | Gradient for the intensity blocks, nonlinearized to boost low end 
                vol_grad :: Double -> Pix
                vol_grad = pix_grad white red . circ_dynamize . (*3)
                
                -- | In-frame normalized volume data for intensity bar
                ds_nf :: [Double]
                ds_nf = in_frame_normalize ds'
                
                -- | Discretized history and frequency values
                ms, ds :: [Int]
                ms              = map (disc 32) . take num_bars $ ms'
                ds              = map (disc 32 . circ_dynamize) . take num_bars $ ds'
                
                -- | Determines which bar pixel x is a part of
                bar :: Int -> Int
                bar x           = (x - x_pad) `div` wt_bar


-- | Colours
white :: Pix
white               = PixelRGB8 255 255 255
light_green         = PixelRGB8 69 230 73
green_yellow        = PixelRGB8 255 255 0
red                 = PixelRGB8 255 0 0



-- | Computes a rainbow colour with some continuously varying offset delta in discrete 
-- |    units (either pixels for space or ticks for time)
rainbow_cts :: Int -> Pix
rainbow_cts delta = PixelRGB8 (mc delta') (mc (delta' + 2*pi /3)) (mc (delta' + 4*pi/3))
    where -- | compute the magnitude of one component (shifted, zeroed, cosine wave)
          mc :: Double -> Pixel8
          mc x = round . max 0 . (*255) . (*0.5) . (+1) . cos . min pi . (-pi+) $ mod' x (2 * pi)
          
          -- | rescale input for a more gradual visual effect
          delta' = (fromIntegral delta) / 28


-- | Computes a linear gradient from the one pixel to another, indexed by a double in [0,1]
pix_grad :: PixelRGB8 -> PixelRGB8 -> Double -> PixelRGB8 
pix_grad (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) d = PixelRGB8 (grad r1 r2) (grad g1 g2) (grad b1 b2) 
    where grad v1 v2 = round $ (fromIntegral v1 * (1-d) + fromIntegral v2 * d)


-- | discretizes a double between 0 and 1 to an integer between 0 and (n-1) inclusive
-- | clamping numbers outside the region
disc :: Int -> Double -> Int
disc n d = max 0 . min (n-1) $ floor (fromIntegral (n-1) * d)


-- | makes a double value more dynamic by mapping with the unit ciricle function
-- | just a little bump to the derivative on the low amplitude range goes a long way!
circ_dynamize :: Double -> Double 
circ_dynamize x = max 0.05 . min 1 $ ((x*(2-x))**0.5 + x) / 2




{- 
 - DEBUG
 -}

-- | test print a frame with dummy data to a file to preview changes without re-rendering an entire video
__tp :: IO()
__tp = writePng "./test_image.png" (bar_plot_480_48 ([fromIntegral i / 48 | i <- [0..47]], 
                                    75, 
                                    100, 
                                    [0.25 + ((fromIntegral i / 48) + 1) / 2| i <- [0..47] ]))

