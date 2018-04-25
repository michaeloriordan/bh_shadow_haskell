module Type_Defs
( Scalar 
, Vec1
, Vec2
, Vec3
, components
) where

type Scalar = Double
type Vec1   = [Double]
type Vec2   = [[Double]]
type Vec3   = [[[Double]]]

components :: Vec1 -> (Scalar, Scalar, Scalar, Scalar)
components v = (x0,x1,x2,x3) where
    (x0:x1:x2:x3:_) = v
