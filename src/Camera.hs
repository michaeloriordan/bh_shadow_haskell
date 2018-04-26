module Camera
( Camera(..)
, Pixel(..)
, Pixels
, init_pixels
) where

import Type_Defs

--------------------------------------------------------------------------------

data Camera = Camera
    { distance    :: Scalar
    , inclination :: Scalar
    , xlimits     :: (Scalar, Scalar)
    , ylimits     :: (Scalar, Scalar)
    , xypixels    :: (Int, Int)
    }

--------------------------------------------------------------------------------

newtype Pixel = Pixel 
    { pixel_xy :: (Scalar, Scalar)
    } deriving (Show)

type Pixels = [Pixel]

init_pixels :: Camera -> Pixels 
init_pixels camera = 
    [ Pixel (x, y)
    | x <- linspace xmin xmax $ fromIntegral (nx-1)
    , y <- linspace ymin ymax $ fromIntegral (ny-1)
    ] where
        linspace a b n = [a, a+(b-a)/n.. b]
        (xmin, xmax) = xlimits camera
        (ymin, ymax) = ylimits camera
        (nx, ny)     = xypixels camera

--------------------------------------------------------------------------------
