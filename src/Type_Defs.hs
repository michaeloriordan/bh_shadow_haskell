module Type_Defs
( Scalar 
, Vec1
, Vec2
, Vec3
, components
, Integrator(..)
, Coords(..)
, vec1_to_string
, vec2_to_string
) where

--------------------------------------------------------------------------------

type Scalar = Double
type Vec1   = [Double]
type Vec2   = [[Double]]
type Vec3   = [[[Double]]]

components :: Vec1 -> (Scalar, Scalar, Scalar, Scalar)
components v = (x0,x1,x2,x3) where
    (x0:x1:x2:x3:_) = v

vec1_to_string :: Vec1 -> String
vec1_to_string v = unwords $ (map show) v

vec2_to_string :: Vec2 -> String
vec2_to_string v = unlines $ map vec1_to_string v

--------------------------------------------------------------------------------

data Integrator = RK4 | VVerlet deriving (Eq)

--------------------------------------------------------------------------------

data Coords = Schwarzschild | Schwarzschild_GP | Kerr_BL | Kerr_KS deriving (Eq)

--------------------------------------------------------------------------------
