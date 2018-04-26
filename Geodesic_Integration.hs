module Geodesic_Integration
( step_geodesic
, stepsize
) where

import Photon
import Type_Defs
import Data.List 
import Geodesic_Equation (dkdl)
import Config(coords, integrator, rh, step_epsilon)

--------------------------------------------------------------------------------

step_geodesic :: Photon -> Scalar -> Photon
step_geodesic = case integrator of
    RK4     -> step_geodesic_rk4
    VVerlet -> step_geodesic_vverlet

--------------------------------------------------------------------------------

stepsize :: Vec1 -> Vec1 -> Scalar
stepsize x k = case coords of
    Kerr_BL -> min (stepsize' x k) (stepsize'' x k)
    _       -> stepsize' x k

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
