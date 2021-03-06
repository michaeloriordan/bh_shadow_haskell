module Config
( camera
, coords
, spin
, rh
, rmax
, rmin
, integrator
, step_epsilon
, nmax
, do_parallel
, chunk_size
) where

import Type_Defs (Scalar, Coords(..), Integrator(..))
import Camera (Camera(..), init_pixels, init_photons)

--------------------------------------------------------------------------------

-- Set up camera far from BH
camera = Camera
    { distance    = 100
    , inclination = (pi/180) * 0
    , xlimits     = (-10, 10)
    , ylimits     = (-10, 10)
    , xypixels    = (1024, 1024)
    , pixels      = init_pixels camera
    , photons     = init_photons k0i camera
    } :: Camera

--------------------------------------------------------------------------------

-- Initial k^0 component of photon momentum
k0i = 10.0 :: Scalar

--------------------------------------------------------------------------------

-- Coordinate system
coords = Kerr_KS :: Coords

--------------------------------------------------------------------------------

-- Black hole spin
spin = 0.9 :: Scalar
rh = 1 + sqrt (1 - spin^2) :: Scalar

--------------------------------------------------------------------------------

-- Radius beyond which photon has escaped
rmax = distance camera + 10 :: Scalar

-- Stop slightly outside horizon in Schwarzschild or Boyer-Lindquist coords
rmin = case coords of 
    Schwarzschild -> rh + 1.0e-6 :: Scalar
    Kerr_BL       -> rh + 1.0e-6 :: Scalar
    _             -> rh          :: Scalar

--------------------------------------------------------------------------------

-- Integration method
integrator = VVerlet :: Integrator

-- Stepsize parameter
step_epsilon = 0.01 :: Scalar

-- Max number of steps before photon considered stuck
nmax = 100000 :: Int

--------------------------------------------------------------------------------

-- Run code in parallel 
do_parallel = True :: Bool

-- Divide tasks into chunks
chunk_size = 128 :: Int

--------------------------------------------------------------------------------
