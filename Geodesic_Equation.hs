module Geodesic_Equation
( dkdl
) where

import Type_Defs
import Geometry (conn)

dot :: Vec1 -> Vec1 -> Scalar
dot x y = sum $ zipWith (*) x y

dot2 :: Vec1 -> Vec1 -> Vec2 -> Scalar
dot2 x y z = dot x $ map (dot y) z 

-- Geodesic equation
dkdl :: Vec1 -> Vec1 -> Vec1
dkdl x k = map (negate . dot2 k k) (conn x)
