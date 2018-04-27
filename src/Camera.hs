module Camera
( Camera(..)
, Pixel(..)
, Pixels
, init_pixels
, init_photons
) where

import Photon
import Type_Defs (Scalar)

--------------------------------------------------------------------------------

data Camera = Camera
    { distance    :: Scalar
    , inclination :: Scalar
    , xlimits     :: (Scalar, Scalar)
    , ylimits     :: (Scalar, Scalar)
    , xypixels    :: (Int, Int)
    , pixels      :: Pixels
    , photons     :: Photons
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

---- Assuming camera far from BH => flat space - Johannsen & Psaltis (2010)
init_photon :: Scalar -> Camera -> Pixel -> Photon
init_photon k0 camera pixel = Photon xi ki where
    cr = distance camera
    ci = inclination camera
    (x, y) = pixel_xy pixel
    sini = sin ci
    cosi = cos ci

    r = sqrt $ x^2 + y^2 + cr^2 
    th = acos $ (y*sini + cr*cosi) / r
    phi = atan2 x $ cr*sini - y*cosi

    k1 = k0 * (-cr / r)
    k2 = k0 * (cosi - (y*sini + cr*cosi) * (cr / r^2)) / 
         (sqrt $ x^2 + (cr*sini - y*cosi)^2)
    k3 = k0 * (x*sini) / (x^2 + (cr*sini - y*cosi)^2)

    xi = [0.0, r, th, phi]
    ki = [k0, k1, k2, k3]

init_photons :: Scalar -> Camera -> Photons
init_photons k0 camera = map (init_photon k0 camera) $ pixels camera

--------------------------------------------------------------------------------
