module Type_Defs
( Scalar 
, Vec1
, Vec2
, Vec3
, components
, Integrator(..)
, Coords(..)
, scalar_to_string
, vec1_to_string
, vec2_to_string
, vec3_to_string
) where

--------------------------------------------------------------------------------

type Scalar = Double
type Vec1   = [Double]
type Vec2   = [[Double]]
type Vec3   = [[[Double]]]

components :: Vec1 -> (Scalar, Scalar, Scalar, Scalar)
components v = (x0,x1,x2,x3) where
    (x0:x1:x2:x3:_) = v

scalar_to_string :: Scalar -> String
scalar_to_string s = show s

vec1_to_string :: Vec1 -> String
vec1_to_string v = unwords $ map scalar_to_string v

vec2_to_string :: Vec2 -> String
vec2_to_string v = unlines $ map vec1_to_string v

vec3_to_string :: Vec3 -> String
vec3_to_string v = unlines $ map vec2_to_string v

--------------------------------------------------------------------------------

data Integrator = RK4 | VVerlet deriving (Eq)

--------------------------------------------------------------------------------

-- Schwarzschild, Gullstrand-Painleve, Boyer-Lindquist, Kerr-Schild
data Coords = Schwarzschild | Schwarzschild_GP | Kerr_BL | Kerr_KS deriving (Eq)

--------------------------------------------------------------------------------
