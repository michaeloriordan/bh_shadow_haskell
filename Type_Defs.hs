module Type_Defs
( Scalar 
, Vec1
, Vec2
, Vec3
, components
, Integrator(..)
, Coords(..)
, Camera(..)
) where

--------------------------------------------------------------------------------

type Scalar = Double
type Vec1   = [Double]
type Vec2   = [[Double]]
type Vec3   = [[[Double]]]

components :: Vec1 -> (Scalar, Scalar, Scalar, Scalar)
components v = (x0,x1,x2,x3) where
    (x0:x1:x2:x3:_) = v

--------------------------------------------------------------------------------

data Integrator = RK4 | VVerlet deriving (Eq)

--------------------------------------------------------------------------------

data Coords = Schwarzschild | Schwarzschild_GP | Kerr_BL | Kerr_KS deriving (Eq)

--------------------------------------------------------------------------------

data Camera = Camera
    { distance    :: Scalar
    , inclination :: Scalar
    , xlimits     :: (Scalar, Scalar)
    , ylimits     :: (Scalar, Scalar)
    , xypixels    :: (Int, Int)
    }

--------------------------------------------------------------------------------
