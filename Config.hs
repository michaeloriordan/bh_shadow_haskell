module Config
( Integrator(..)
, Camera(..)
, camera
, k0_init
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

import Type_Defs
import Geometry (Coords(..))

--------------------------------------------------------------------------------

data Integrator = RK4 | VVerlet deriving (Eq)

data Camera = Camera
    { distance    :: Scalar
    , inclination :: Scalar
    , xlimits     :: (Scalar, Scalar)
    , ylimits     :: (Scalar, Scalar)
    , xypixels    :: (Int, Int)
    }

--------------------------------------------------------------------------------

-- Set up camera far from BH
camera = Camera
    { distance    = 100
    , inclination = (pi/180) * 0
    , xlimits     = (-10, 10)
    , ylimits     = (-10, 10)
    , xypixels    = (1024, 1024)
    }

--------------------------------------------------------------------------------

-- Initial k^0 component of photon momentum
k0_init = 10.0

--------------------------------------------------------------------------------

-- Coordinate system
coords = Kerr_KS

--------------------------------------------------------------------------------

-- Black hole spin
spin = 0.9
rh = 1 + sqrt (1 - spin^2)

--------------------------------------------------------------------------------

-- Radius beyond which photon has escaped
rmax = distance camera + 10

-- Stop slightly outside horizon in Schwarzschild or Boyer-Lindquist coords
rmin = case coords of 
    Kerr_BL -> rh + 1.0e-6
    _       -> rh

--------------------------------------------------------------------------------

-- Integration method
integrator = VVerlet

-- Stepsize parameter
step_epsilon = 0.01

-- Max number of steps before photon considered stuck
nmax = 100000 :: Int

--------------------------------------------------------------------------------

-- Run code in parallel 
do_parallel = True

-- Divide tasks into chunks
chunk_size = 128 :: Int

--------------------------------------------------------------------------------
