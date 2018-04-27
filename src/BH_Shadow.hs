module BH_Shadow
( calculate_shadow
) where

import Photon 
import Camera
import Config (camera)
import Type_Defs (Scalar, Vec2)
import Propagate_Photons (propagate_photons, photon_escaped, photon_captured)

--------------------------------------------------------------------------------

calculate_shadow :: Camera -> Vec2
calculate_shadow camera = results where
    initial_photons = photons camera 
    final_photons   = propagate_photons initial_photons
    results         = data_to_save final_photons camera

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
data_to_save :: Photons -> Camera -> Vec2
data_to_save phs camera = 
    [ [x, y, r, th, phi, stat] 
    | ((x,y), (r,th,phi), stat) <- zip3 xys positions status
    ] where
        positions = map photon_position phs
        status    = map photon_status phs
        xys       = map pixel_xy $ pixels camera

--------------------------------------------------------------------------------
