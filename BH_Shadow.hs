module BH_Shadow
( init_photons
, propagate_photons 
, data_to_save
) where

import Config
import Photon
import Camera
import Type_Defs
import Data.List 
import Geometry (conn)
import Geodesic_Equation (dkdl)
import Control.Parallel.Strategies (withStrategy,parListChunk,rseq)

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

stepsize :: Vec1 -> Vec1 -> Scalar
stepsize x k = case coords of
    Kerr_BL -> min (stepsize' x k) (stepsize'' x k)
    _       -> stepsize' x k

stepsize' :: Vec1 -> Vec1 -> Scalar
stepsize' x k = dl where
    (_,x1, _, _) = components x
    (_,k1,k2,k3) = components k
    d1 = abs k1 / x1
    d2 = abs k2
    d3 = abs k3
    dl = step_epsilon / (d1 + d2 + d3)

stepsize'' :: Vec1 -> Vec1 -> Scalar
stepsize'' x k = dl where
    (_,x1,_,_) = components x
    (_,k1,_,_) = components k
    dl = (x1 - rh) / (2 * abs k1)

--------------------------------------------------------------------------------

step_geodesic_rk4 :: Photon -> Scalar -> Photon
step_geodesic_rk4 ph dl = phf where
    x = photon_x ph
    k = photon_k ph

    dx1 = k
    dk1 = dkdl x k

    x1 = zipWith (\a b -> a + (dl/2)*b) x dx1 
    k1 = zipWith (\a b -> a + (dl/2)*b) k dk1 

    dx2 = k1
    dk2 = dkdl x1 k1

    x2 = zipWith (\a b -> a + (dl/2)*b) x dx2 
    k2 = zipWith (\a b -> a + (dl/2)*b) k dk2 

    dx3 = k2
    dk3 = dkdl x2 k2

    x3 = zipWith (\a b -> a + dl*b) x dx3 
    k3 = zipWith (\a b -> a + dl*b) k dk3 

    dx4 = k3
    dk4 = dkdl x3 k3

    dx = zipWith4 (\a b c d -> (dl/6) * (a + 2*b + 2*c + d)) dx1 dx2 dx3 dx4 
    dk = zipWith4 (\a b c d -> (dl/6) * (a + 2*b + 2*c + d)) dk1 dk2 dk3 dk4 

    xf = zipWith (+) x dx
    kf = zipWith (+) k dk

    phf = Photon xf kf

step_geodesic_vverlet :: Photon -> Scalar -> Photon
step_geodesic_vverlet ph dl = phf where
    x = photon_x ph
    k = photon_k ph

    dk1 = dkdl x k

    xf = zipWith3 (\a b c -> a + dl*b + ((dl^2)/2)*c) x k dk1 

    dk2 = dkdl xf k1 
        where k1 = zipWith (\a b -> a + dl*b) k dk1 -- Estimate new k

    kf = zipWith3 (\a b c -> a + (dl/2)*(b + c)) k dk1 dk2 

    phf = Photon xf kf

step_geodesic :: Photon -> Scalar -> Photon
step_geodesic = case integrator of
    RK4     -> step_geodesic_rk4
    VVerlet -> step_geodesic_vverlet

step_photon :: Photon -> Photon
step_photon ph = phf where
    dl = stepsize (photon_x ph) (photon_k ph)
    phh = step_geodesic ph dl
    phf = bound_coords phh

--------------------------------------------------------------------------------

bound_coords :: Photon -> Photon
bound_coords ph = case coords of
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

photon_finished :: Photon -> Bool
photon_finished ph = (photon_escaped ph) || (photon_captured ph)

photon_escaped :: Photon -> Bool
photon_escaped ph = (photon_r ph) > rmax

photon_captured :: Photon -> Bool
photon_captured ph = (photon_r ph) <= rmin

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

propagate_photons :: Photons -> Photons
propagate_photons = map' propagate_photon

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
data_to_save' :: Photons -> Pixels -> Vec2
data_to_save' phs pixels = 
    [ [x, y, r, th, phi, stat] 
    | ((x,y), (r,th,phi), stat) <- zip3 xys positions status
    ] where
        positions = map photon_position phs
        status    = map photon_status phs
        xys       = map pixel_xy pixels

--------------------------------------------------------------------------------

data_to_save :: Photons -> Pixels -> String
data_to_save phs pixels = data_to_string $ data_to_save' phs pixels

data_to_string :: Vec2 -> String
data_to_string d = unlines $ map (unwords . map show) d

--------------------------------------------------------------------------------
