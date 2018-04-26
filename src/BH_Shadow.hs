module BH_Shadow
( calculate_shadow
) where

import Config
import Photon
import Camera
import Type_Defs
import Propagate_Photons (propagate_photons, photon_escaped, photon_captured)

--------------------------------------------------------------------------------

calculate_shadow :: Camera -> Vec2
calculate_shadow camera = results where
    pixels          = init_pixels camera
    initial_photons = init_photons pixels
    final_photons   = propagate_photons initial_photons
    results         = data_to_save final_photons pixels

--------------------------------------------------------------------------------

-- Assuming camera far from BH => flat space - Johannsen & Psaltis (2010)
init_photon :: Scalar -> Scalar -> Scalar -> Pixel -> Photon
init_photon k0 cr ci pixel = Photon xi ki where
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

init_photons :: Pixels -> Photons
init_photons = map $ init_photon k0_init (distance camera) (inclination camera)

--------------------------------------------------------------------------------

photon_status :: Photon -> Scalar
photon_status ph
    | photon_captured ph = 0
    | photon_escaped ph  = 1
    | otherwise          = -1

-- Save: "x y r th phi status" 
-- Initial pixel: (x, y)
-- Final position: (r, th, phi)
-- Status: escaped, captured, or stuck
data_to_save :: Photons -> Pixels -> Vec2
data_to_save phs pixels = 
    [ [x, y, r, th, phi, stat] 
    | ((x,y), (r,th,phi), stat) <- zip3 xys positions status
    ] where
        positions = map photon_position phs
        status    = map photon_status phs
        xys       = map pixel_xy pixels

--------------------------------------------------------------------------------
