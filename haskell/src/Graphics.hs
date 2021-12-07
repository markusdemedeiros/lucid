module Graphics (bar_plot_480, ImageMetadata, defaultImageMetadata, Frame) where


import Data (in_frame_normalize)

import Util

import Data.Fixed (mod')

-- Static graphics library (JuicyPixels) for individual frame drawing
import Codec.Picture


import Control.Monad (foldM, mplus)


{- 
 - Graphics for the bar chart
 -}


data ImageMetadata = ImageMetadata { width :: Int
                                   , height :: Int
                                   }

defaultImageMetadata = ImageMetadata 1280 720 


-- We only deal with one (fixed) pixel type
type Pix = PixelRGB8
type Frame = Image Pix



-- bar_plot :: ImageMetadata -> ([Double], Int) -> Frame
-- bar_plot md (ds, t) = generateImage f (width md) (height md)
--     where f x y 
--             | (ds!!nbar) > 1 = error "out of range value check"
--             | (height md - y - 20) <= hbar = rainbow!!mag
--             | otherwise = (PixelRGB8 (fromIntegral (x+t) `mod` 255) 
--                                      (fromIntegral (y+3*round(100 * sin(fromIntegral t / 40))) `mod` 255) 
--                                      (fromIntegral(x*y`div`30) `mod` 255))
--             where nbar = ((length ds) * x) `div` (width md)
--                   hbar = round $ 0.3* (fromIntegral nbar) * (ds!!nbar)*(fromIntegral(height md))
--                   mag = (round (fromIntegral nbar + (fromIntegral t) / 30)) `mod` lr
--                   rainbow = [ PixelRGB8 255 0 0
--                             , PixelRGB8 255 127 0
--                             , PixelRGB8 255 255 0
--                             , PixelRGB8 127 255 0
--                             , PixelRGB8 0 255 0
--                             , PixelRGB8 0 255 127
--                             , PixelRGB8 0 255 255
--                             , PixelRGB8 0 127 255
--                             , PixelRGB8 0 0 255
--                             , PixelRGB8 120 0 255
--                             , PixelRGB8 255 0 255
--                             , PixelRGB8 255 0 127 ]
--                   lr = length rainbow



-- Produces 640x480 bar plot of 48 bars
bar_plot_480 :: ([Double], Int, Int, [Double]) -> Frame
bar_plot_480 (ds', t, num_f, ms') = generateImage f wt ht 
        where   f x y 
                    | (ht-y <= 23) && (21 <= ht-y) && (x > 32) && (x < 32 + round ((fromIntegral (wt-64))*(fromIntegral t)/fromIntegral num_f))
                            = rainbow_cts (x - t)
                    | (ht-y == 22) && (x > 32) && (x < wt-32)
                            = PixelRGB8 200 200 200 
                    | ((y-32) >= 0) 
                            && (ht-y>=56) 
                            && (x > x_pad) 
                            && (wt-x>x_pad)
                            && (ht-56-y)`mod` 12 < 2
                            = wht
                    | (y > ht - 32) || ((y>ht-56)&&(y<ht-48)) || (y < 40)
                            = wht
                    | (x <= x_pad) || ( (wt-x) <= x_pad)
                            = wht
                    | (x - x_pad + 1) `mod` wt_bar < 2
                            = wht
                    | y >= ht-48
                            = vol_grad (ds_nf!!(bar x))
                    | y > ht-56-(ds!!(bar x))*12
                            = bar_grad $ (fromIntegral (ht-y-32))/(fromIntegral (ht-32-56))
                    | (y >= ht-56-(ms!!(bar x))*12) -- && (y > ht-56-(1+(ms!!(bar x)))*12) -- && (((x + y) `mod` 2) == 0) 
                            = PixelRGB8 204 255 156
                    | otherwise 
                            = if (((x + y) `mod` 3) == 0) 
                                then PixelRGB8 220 240 220 -- bar_grad (ds!!(bar x))
                                else wht
                            -- = PixelRGB8 0  (round((ds!!(bar x))*2048)) (round((ds!!(bar x))*255))
                wt = 640
                ht = 480

                ds_nf = in_frame_normalize ds'
                
                -- Computes a rainbow colour with some continuously varying offset
                rainbow_cts :: Int -> PixelRGB8
                rainbow_cts delta = PixelRGB8 (mc delta') (mc (delta' + 2*pi /3)) (mc (delta' + 4*pi/3))
                    where mc :: Double -> Pixel8
                          mc x = round . max 0 . (*255) $ (*0.5) . (+1) . cos . min pi . (-pi+) $ mod' x (2 * pi)
                          delta' = (fromIntegral delta) / 28
              
                num_bars        = 48
                x_pad           = 32
                levels          = 32
                wt_bar          = 12
                 
                bar_grad :: Double -> PixelRGB8 
                bar_grad = pix_grad (PixelRGB8 69 230 73) (PixelRGB8 255 255 0) -- (PixelRGB8 220 227 91) 
                
                vol_grad = pix_grad (PixelRGB8 255 255 255) (PixelRGB8 255 0 0) . circ_dynamize . (*3) -- (PixelRGB8 220 227 91) 
                -- pix_grad (PixelRGB8 255 0 0) (PixelRGB8 0 0 255)
                -- pix_grad (PixelRGB8 255 94 98) (PixelRGB8 255 153 102) 

                wht             = PixelRGB8 255 255 255
                gry             = PixelRGB8 190 199 199
                red             = PixelRGB8 187 32 32
                blu             = PixelRGB8 6 17 28
                ylw             = PixelRGB8 255 235 77
                blk             = PixelRGB8 18 19 23

                -- 0 gives the left pixel
                -- 1 gives the right pixel
                pix_grad :: PixelRGB8 -> PixelRGB8 -> Double -> PixelRGB8 
                pix_grad (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) d = PixelRGB8 (grad r1 r2) (grad g1 g2) (grad b1 b2) 
                    where grad v1 v2 = round $ (fromIntegral v1 * (1-d) + fromIntegral v2 * d)

                ms              = map (disc 32) . take num_bars $ ms'
                ds              = map (disc 32 . circ_dynamize) . take num_bars $ ds'
                bar x           = (((x - x_pad) `div` wt_bar))  --`mod` num_bars




__tp :: IO()
__tp = writePng "./test_image.png" (bar_plot_480 ([fromIntegral i / 48 | i <- [0..47]], 75, 100, [0.25 + ((fromIntegral i / 48) + 1) / 2| i <- [0..47] ] ))

-- disc n discretizes a double between 0 and 1 to an integer between 0 and (n-1) inclusive
--  clamping numbers outside the region
disc :: Int -> Double -> Int
disc n d = max 0 . min (n-1) $ floor (fromIntegral (n-1) * d)

-- makes a double more dynamic by mapping with the unit ciricle function
-- just a little bump to the derivative on the low amplitude range goes a long way
circ_dynamize :: Double -> Double 
circ_dynamize x = min 1 $ ((x*(2-x))**0.5 + 2*x) / 3




