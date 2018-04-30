module Propagate_Photons
( propagate
, gather_results
) where

import Config
import Photon
import Camera
import Type_Defs
import Geodesic_Integration (step_geodesic)
import Control.Parallel.Strategies (withStrategy,parListChunk,rseq)

--------------------------------------------------------------------------------

propagate :: Photons -> Photons
propagate = map' propagate_photon

--------------------------------------------------------------------------------

-- Save: "x y r th phi status" 
-- Initial pixel: (x, y)
-- Final position: (r, th, phi)
-- Status: escaped, captured, or stuck
gather_results :: Photons -> Camera -> Vec2
gather_results phs camera = 
    [ [x, y, r, th, phi, stat] 
    | ((x,y), (r,th,phi), stat) <- zip3 xys positions status
    ] where
        positions = map photon_position phs
        status    = map photon_status phs
        xys       = map pixel_xy $ pixels camera

--------------------------------------------------------------------------------

photon_finished :: Photon -> Bool
photon_finished ph = (photon_escaped ph) || (photon_captured ph)

photon_escaped :: Photon -> Bool
photon_escaped ph = (photon_r ph) > rmax

photon_captured :: Photon -> Bool
photon_captured ph = (photon_r ph) <= rmin

photon_status :: Photon -> Scalar
photon_status ph
    | photon_captured ph = 0
    | photon_escaped ph  = 1
    | otherwise          = -1

--------------------------------------------------------------------------------

step_photon :: Photon -> Photon
step_photon ph = phf where
    phh = step_geodesic ph
    phf = bound_coords phh

--------------------------------------------------------------------------------

parmap :: (a -> b) -> [a] -> [b]
parmap = parmap' chunk_size

parmap' :: Int -> (a -> b) -> [a] -> [b]
parmap' chunk f = withStrategy (parListChunk chunk rseq) . map f

map' :: (a -> b) -> [a] -> [b]
map' = if do_parallel then parmap else map

--------------------------------------------------------------------------------

propagate_photon' :: Int -> Photon -> Photon
propagate_photon' n ph 
    | photon_finished ph || n > nmax = ph
    | otherwise = propagate_photon' (n+1) $ step_photon ph

propagate_photon :: Photon -> Photon
propagate_photon = propagate_photon' 0

--------------------------------------------------------------------------------

bound_coords :: Photon -> Photon
bound_coords ph = case coords of
    Schwarzschild    -> bound_coords' (photon_x ph) (photon_k ph)
    Schwarzschild_GP -> bound_coords' (photon_x ph) (photon_k ph)
    Kerr_BL          -> bound_coords' (photon_x ph) (photon_k ph)
    Kerr_KS          -> bound_coords' (photon_x ph) (photon_k ph)

-- Assumes x2 and x3 usual theta and phi
-- Force theta to stay in the domain [0, pi] - Chan et al. (2013)
bound_coords' :: Vec1 -> Vec1 -> Photon
bound_coords' x k  
    | x2 > pi   = Photon [x0, x1, 2*pi-x2, x3+pi] [k0, k1, -k2, k3]
    | x2 < 0    = Photon [x0, x1,     -x2, x3-pi] [k0, k1, -k2, k3]
    | otherwise = Photon [x0, x1,      x2,    x3] [k0, k1,  k2, k3]
    where
        (x0,x1,x2,x3) = components x
        (k0,k1,k2,k3) = components k

--------------------------------------------------------------------------------
