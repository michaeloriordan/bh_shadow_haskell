module Photon
( Photon(..)
, Photons
, photon_r
, photon_position
) where

import Type_Defs

data Photon = Photon 
    { photon_x :: Vec1
    , photon_k :: Vec1
    } deriving (Show)

photon_r :: Photon -> Scalar
photon_r ph = r where
    (r,_,_) = photon_position ph

photon_position :: Photon -> (Scalar, Scalar, Scalar)
photon_position ph = (r,th,phi) where
    (_,r,th,phi) = components $ photon_x ph 

type Photons = [Photon]
