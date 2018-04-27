module BH_Shadow
( calculate_shadow
) where

import Photon 
import Camera
import Config (camera)
import Type_Defs (Scalar, Vec2)
import Propagate_Photons (propagate_photons, gather_results)

--------------------------------------------------------------------------------

calculate_shadow :: Camera -> Vec2
calculate_shadow camera = results where
    initial_photons = photons camera 
    final_photons   = propagate_photons initial_photons
    results         = gather_results final_photons camera

--------------------------------------------------------------------------------
