module Noise where

import           Codec.Picture
import           Math.Noise
import           Data.Maybe (fromJust)

noise =
  perlin
    { perlinFrequency = 4
    , perlinOctaves   = 6
    }

sample x z =
    let
      c = floor . (*128) . clamp 0 1 . (*0.5) . (+1) . fromJust
        $ getValue perlin (fi x/128, 0, fi z/128)
    in
      PixelRGBA8 0 0 0 c
  where
    clamp l h = max l . min h
    fi :: Int -> Double
    fi = fromIntegral

main =
  savePngImage "noise.png" (ImageRGBA8 $ generateImage sample 2080 2080)

