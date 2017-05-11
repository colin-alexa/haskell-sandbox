module Main where

import qualified Codec.Picture              as JP
import qualified Codec.Picture.Types        as JP

-- | content-aware cropping
-- $> crop <image> <width> <height>
--
-- resize an image by removing/duplicating 'uninteresting' paths
-- rather than by scaling or cropping
-- 
-- to remove 1px from the width of a picture
-- find the lowest-scoring path from the top of the image to the bottom
-- and delete it
--
-- a path is a list of positions where Next(i, j) ~= (i+1, j+-1 | j)
-- But first...some basics


main :: IO ()
main = JP.writePng a $ JP.generateImage pixelRenderer 256 256
  where pixelRenderer x y = JP.PixelRGB8 (255 - fromIntegral x) (255 - fromIntegral y) 128

a :: String
a = "hello.png"
