module BH_Shadow
( calculate_shadow
) where

import Config (camera)
import Type_Defs (Vec2)
import Camera (Camera(..))
import Propagate_Photons (propagate_photons, gather_results)

--------------------------------------------------------------------------------

calculate_shadow :: Camera -> Vec2
calculate_shadow camera = results where
    initial_photons = photons camera 
    final_photons   = propagate_photons initial_photons
    results         = gather_results final_photons camera

--------------------------------------------------------------------------------
