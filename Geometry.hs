module Geometry
( gcov
, gcon
, conn
, Coords(..)
) where

import Type_Defs

--------------------------------------------------------------------------------

data Coords = Schwarzschild | Schwarzschild_GP | Kerr_BL | Kerr_KS deriving (Eq)

--------------------------------------------------------------------------------

gcov :: Coords -> Scalar -> Vec1 -> Vec2 
gcov coords a = case coords of
    Schwarzschild    -> gcov_schwarzschild
    Schwarzschild_GP -> gcov_schwarzschild_GP
    Kerr_BL          -> gcov_kerr_BL a
    Kerr_KS          -> gcov_kerr_KS a

gcon :: Coords -> Scalar -> Vec1 -> Vec2 
gcon coords a = case coords of
    Schwarzschild    -> gcon_schwarzschild
    Schwarzschild_GP -> gcon_schwarzschild_GP
    Kerr_BL          -> gcon_kerr_BL a
    Kerr_KS          -> gcon_kerr_KS a

conn :: Coords -> Scalar -> Vec1 -> Vec3
conn coords a = case coords of
    Schwarzschild    -> conn_schwarzschild
    Schwarzschild_GP -> conn_schwarzschild_GP
    Kerr_BL          -> conn_kerr_BL a
    Kerr_KS          -> conn_kerr_KS a

--------------------------------------------------------------------------------

gcov_schwarzschild :: Vec1 -> Vec2 
gcov_schwarzschild x = g where
    (_,r,th,_) = components x
    r2 = r^2
    b = 1 - (2 / r)
    sth2 = (sin th)^2

    g00 = -b
    g11 = 1 / b
    g22 = r2
    g33 = r2 * sth2

    g = [[g00,   0,   0,   0],
         [  0, g11,   0,   0],
         [  0,   0, g22,   0],
         [  0,   0,   0, g33]]

gcon_schwarzschild :: Vec1 -> Vec2 
gcon_schwarzschild x = g where
    (_,r,th,_) = components x
    r2 = r^2
    b = 1 - (2 / r)
    sth2 = (sin th)^2

    g00 = -1 / b
    g11 = b
    g22 = 1 / r2
    g33 = 1 / (r2 * sth2)

    g = [[g00,   0,   0,   0],
         [  0, g11,   0,   0],
         [  0,   0, g22,   0],
         [  0,   0,   0, g33]]

conn_schwarzschild :: Vec1 -> Vec3 
conn_schwarzschild x = c where
    (_,r,th,_) = components x
    b = r - 2
    sth = sin th
    cth = cos th
    sth2 = sth^2
    r3 = r^3

    ----------------------------------------

    c010 = 1 / (r * b)
    c001 = c010

    ----------------------------------------

    c100 = b / r3
    c111 = -1 / (r * b)
    c122 = -b
    c133 = -b * sth2

    ----------------------------------------

    c221 = 1 / r
    c233 = -cth * sth
    c212 = c221

    ----------------------------------------

    c331 = 1 / r
    c332 = cth / sth;
    c313 = c331 
    c323 = c332 

    ----------------------------------------
    
    c = [[[   0,c001,0,0], [c010,   0,   0,   0], [0,   0,   0,   0], [0,   0,   0,   0]],
         [[c100,   0,0,0], [   0,c111,   0,   0], [0,   0,c122,   0], [0,   0,   0,c133]],
         [[   0,   0,0,0], [   0,   0,c212,   0], [0,c221,   0,   0], [0,   0,   0,c233]],
         [[   0,   0,0,0], [   0,   0,   0,c313], [0,   0,   0,c323], [0,c331,c332,   0]]]

--------------------------------------------------------------------------------

gcov_schwarzschild_GP :: Vec1 -> Vec2 
gcov_schwarzschild_GP x = g where
    (_,r,th,_) = components x
    r2 = r^2
    b = 1 - (2 / r)
    sth2 = (sin th)^2

    g00 = -b
    g11 = 1
    g22 = r2
    g33 = r2 * sth2
    g01 = sqrt (2 / r)
    g10 = g01

    g = [[g00, g01,   0,   0],
         [g10, g11,   0,   0],
         [  0,   0, g22,   0],
         [  0,   0,   0, g33]]

gcon_schwarzschild_GP :: Vec1 -> Vec2 
gcon_schwarzschild_GP x = g where
    (_,r,th,_) = components x
    r2 = r^2
    b = 1 - (2 / r)
    sth2 = (sin th)^2

    g00 = -1
    g11 = b
    g22 = 1 / r2
    g33 = 1 / (r2 * sth2)
    g01 = sqrt (2 / r)
    g10 = g01

    g = [[g00, g01,   0,   0],
         [g10, g11,   0,   0],
         [  0,   0, g22,   0],
         [  0,   0,   0, g33]]

conn_schwarzschild_GP :: Vec1 -> Vec3 
conn_schwarzschild_GP x = c where
    (_,r,th,_) = components x
    b = sqrt (2 / r)
    br = b * r
    sth = sin th
    cth = cos th
    sth2 = sth^2

    r2 = r^2;
    r3 = r^3;

    ----------------------------------------

    c000 = b / r2
    c010 = 1 / r2
    c001 = c010
    c011 = 1 / (br * r)
    c022 = -br
    c033 = -br * sth2

    ----------------------------------------

    c100 = (r - 2) / r3
    c110 = - b / r2
    c101 = c110
    c111 = -1 / r2
    c122 = 2 - r
    c133 = -(r - 2) * sth2

    ---------------------------------------- 

    c221 = 1 / r
    c212 = c221
    c233 = -cth * sth

    ----------------------------------------

    c331 = 1 / r
    c313 = c331
    c332 = cth / sth
    c323 = c332

    ----------------------------------------  

    c = [[[c000,c001,0,0], [c010,c011,   0,   0], [0,   0,c022,   0], [0,   0,   0,c033]],
         [[c100,c101,0,0], [c110,c111,   0,   0], [0,   0,c122,   0], [0,   0,   0,c133]],
         [[   0,   0,0,0], [   0,   0,c212,   0], [0,c221,   0,   0], [0,   0,   0,c233]],
         [[   0,   0,0,0], [   0,   0,   0,c313], [0,   0,   0,c323], [0,c331,c332,   0]]]

--------------------------------------------------------------------------------

delta_kerr :: Scalar -> Scalar -> Scalar
delta_kerr a r = r^2 - 2*r + a^2

sigma_kerr :: Scalar -> Scalar -> Scalar -> Scalar
sigma_kerr a r th = r^2 + (a^2) * (cos th)^2

gcov_kerr_BL :: Scalar -> Vec1 -> Vec2
gcov_kerr_BL a x = g where
    (_,r,th,_) = components x
    sigma = sigma_kerr a r th
    delta = delta_kerr a r
    sth2 = (sin th)^2
    r2 = r^2
    a2 = a^2
    b = 2 * r / sigma

    g00 = -(1 - b)
    g11 = sigma / delta
    g22 = sigma
    g33 = (r2 + a2 + b * a2 * sth2) * sth2
    g03 = - b * a * sth2
    g30 = g03

    g = [[g00,   0,   0, g03], 
         [  0, g11,   0,   0], 
         [  0,   0, g22,   0], 
         [g30,   0,   0, g33]]

gcon_kerr_BL :: Scalar -> Vec1 -> Vec2
gcon_kerr_BL a x = g where
    (_,r,th,_) = components x
    sigma = sigma_kerr a r th
    delta = delta_kerr a r
    sth2 = (sin th)^2
    r2 = r^2
    a2 = a^2

    a_kerr = (r2 + a2) * (r2 + a2) - a2 * delta * sth2

    g00 = -a_kerr / (sigma * delta)
    g11 = delta / sigma
    g22 = 1 / sigma
    g33 = (delta - a2 * sth2) / (sigma * delta * sth2)
    g03 = - 2 * a * r / (delta * sigma)
    g30 = g03

    g = [[g00,   0,   0, g03], 
         [  0, g11,   0,   0], 
         [  0,   0, g22,   0], 
         [g30,   0,   0, g33]]

conn_kerr_BL :: Scalar -> Vec1 -> Vec3
conn_kerr_BL a x = c where
    (_,r,th,_) = components x
    delta = delta_kerr a r
    sigma = sigma_kerr a r th

    cth = cos th
    sth = sin th
    sth2 = sth^2
    cth2 = cth^2
    sth3 = sth^3

    cotth = cth / sth

    r2 = r^2
    a2 = a^2
    a3 = a^3
    a4 = a^4

    sigma_m = r2 - a2 * cth2

    sigma2 = sigma^2
    sigma3 = sigma^3

    a_kerr = (r2 + a2) * (r2 + a2) - a2 * delta * sth2

    ---------------------------------------- 
    
    c010 = (r2 + a2) * sigma_m / (sigma2 * delta)
    c001 = c010

    c020 = -2 * a2 * r * sth * cth / sigma2
    c002 = c020

    c031 = a * sth2 * (a2 * cth2 * (a2 - r2) - r2 * (a2 + 3 * r2)) / (sigma2 * delta)
    c013 = c031

    c032 = 2 * a3 * r * sth3 * cth / sigma2
    c023 = c032

    ---------------------------------------- 
    
    c100 = delta * sigma_m / sigma3

    c111 = (r * a2 * sth2 - sigma_m) / (sigma * delta)

    c121 = - a2 * sth * cth / sigma
    c112 = c121

    c122 = - r * delta / sigma

    c130 = - delta * a * sth2 * sigma_m / sigma3
    c103 = c130

    c133 = (delta * sth2 / sigma3) * (-r * sigma2 + a2 * sth2 * sigma_m)

    ---------------------------------------- 
    
    c200 = -2 * a2 * r * sth * cth / sigma3

    c211 = a2 * sth * cth / (sigma * delta)

    c221 = r / sigma
    c212 = c221

    c222 = - a2 * sth * cth / sigma

    c230 = 2 * a * r * (r2 + a2) * sth * cth / sigma3
    c203 = c230

    c233 = - (sth * cth / sigma3) * (a_kerr * sigma + 2 * (r2 + a2) * a2 * r * sth2)

    ----------------------------------------
    
    c310 = a * sigma_m / (sigma2 * delta)
    c301 = c310

    c320 = -2 * a * r * cotth / sigma2
    c302 = c320

    c331 = (r * sigma2 + a4 * sth2 * cth2 - r2 * (sigma + r2 + a2)) / (sigma2 * delta)
    c313 = c331

    c332 = (cotth / sigma2) * (sigma2 + 2 * a2 * r * sth2)
    c323 = c332

    ----------------------------------------
    
    c = [[[   0,c001,c002,   0], [c010,   0,   0,c013], [c020,   0,   0,c023], [   0,c031,c032,   0]],
         [[c100,   0,   0,c103], [   0,c111,c112,   0], [   0,c121,c122,   0], [c130,   0,   0,c133]],
         [[c200,   0,   0,c203], [   0,c211,c212,   0], [   0,c221,c222,   0], [c230,   0,   0,c233]],
         [[   0,c301,c302,   0], [c310,   0,   0,c313], [c320,   0,   0,c323], [   0,c331,c332,   0]]]

--------------------------------------------------------------------------------

gcov_kerr_KS :: Scalar -> Vec1 -> Vec2
gcov_kerr_KS a x = g where
    (_,r,th,_) = components x
    sigma = sigma_kerr a r th
    sth2 = (sin th)^2
    b = 2 * r / sigma

    g00 = - (1 - b)
    g11 = 1 + b
    g22 = sigma
    g33 = (sigma + a^2 * (1 + b) * sth2) * sth2
    g01 = b
    g10 = g01
    g03 = - b * a * sth2
    g30 = g03
    g13 = - a * (1 + b) * sth2
    g31 = g13

    g = [[g00, g01,   0, g03], 
         [g10, g11,   0, g13], 
         [  0,   0, g22,   0], 
         [g30, g31,   0, g33]]

gcon_kerr_KS :: Scalar -> Vec1 -> Vec2
gcon_kerr_KS a x = g where
    (_,r,th,_) = components x
    sigma = sigma_kerr a r th
    delta = delta_kerr a r
    sth2 = (sin th)^2
    b = 2 * r / sigma

    g00 = - (1 - b)
    g11 = delta / sigma
    g22 = 1 / sigma
    g33 = 1 / (sigma * sth2)
    g01 = b
    g10 = g01
    g13 = a / sigma
    g31 = g13

    g = [[g00, g01,   0,   0], 
         [g10, g11,   0, g13], 
         [  0,   0, g22,   0], 
         [  0, g31,   0, g33]]

conn_kerr_KS :: Scalar -> Vec1 -> Vec3
conn_kerr_KS a x = c where
    (_,r,th,_) = components x
    sigma = sigma_kerr a r th
    delta = delta_kerr a r
    b = 2 * r / sigma

    cth = cos th
    sth = sin th
    sth2 = sth^2
    cth2 = cth^2
    sth3 = sth^3
    c2th = cos (2 * th)
    s2th = sin (2 * th)

    cth4 = cth^4
    sth4 = sth^4

    cotth = cth / sth

    r2 = r^2
    r3 = r^3
    a2 = a^2
    a3 = a^3
    a4 = a^4

    sigma_m = r2 - a2 * cth2

    sigma2 = sigma^2
    sigma3 = sigma^3

    ----------------------------------------
    
    c000 = 2 * r * sigma_m / sigma3

    c001 = sigma_m * (2 * r + sigma) / sigma3
    c010 = c001

    c011 = 2 * sigma_m * (r + sigma) / sigma3

    c002 = - 2 * a2 * r * cth * sth / sigma2
    c020 = c002

    c012 = - 2 * a2 * r * cth * sth / sigma2
    c021 = c012

    c022 = - b * r

    c003 = - 2 * a * r * sigma_m * sth2 / sigma3
    c030 = c003

    c013 = - a * sigma_m * (2 * r + sigma) * sth2 / sigma3
    c031 = c013

    c023 = 2 * a3 * r * cth * sth3 / sigma2
    c032 = c023

    c033 = 2 * r * sth2 * (-r * sigma2 + a2 * sigma_m * sth2) / sigma3

    ---------------------------------------- 
   
    c100 = delta * sigma_m / sigma3

    c101 = sigma_m * (-2 * r + a2 * sth2) / sigma3
    c110 = c101

    c111 = - sigma_m * (r * (2 + r) + a2 * c2th) / sigma3

    c112 = - a2 * s2th / (a2 + 2 * r2 + a2 * c2th)
    c121 = c112

    c122 = - r * delta / sigma

    c103 = - a * delta * sigma_m * sth2 / sigma3
    c130 = c103

    c113 = (a4 * r * cth4
            + r2 * (2 * r + r3 - a2 * sth2)
            + a2 * cth2 * (2 * r * (r2 - 1) + a2 * sth2))
            * a * sth2 / sigma3
    c131 = c113

    c133 = - delta * sth2 * (r * sigma2 - a2 * sigma_m * sth2) / sigma3

    ---------------------------------------- 

    c200 = - 2 * a2 * r * cth * sth / sigma3

    c210 = c200
    c201 = c210

    c211 = c200

    c212 = r / sigma
    c221 = c212

    c222 = - a2 * cth * sth / sigma

    c203 = a * r * (a2 + r2) * s2th / sigma3
    c230 = c203

    c213 = (r3 * (2 + r)
            + a2 * (2 * r * (1 + r) * cth2 + a2 * cth4 + 2 * r * sth2))
            * a * cth * sth / sigma3
    c231 = c213

    c233 = (-1 - (a2 * (3 * a2 * r + r3 * (4 + r) + a2 * (2 * r2 * cth2
                 + a2 * cth4 + r * c2th)) * sth2) / sigma3) * cth * sth

    ---------------------------------------- 

    c300 = a * sigma_m / sigma3

    c310 = c300
    c301 = c310

    c311 = c300

    c302 = - 2 * a * r * cotth / sigma2
    c320 = c302

    c312 = - a * (2 * r + sigma) * cotth / sigma2
    c321 = c312

    c322 = - a * r / sigma

    c303 = - a2 * sigma_m * sth2 / sigma3
    c330 = c303

    c313 = (r * sigma2 - a2 * sigma_m * sth2) / sigma3
    c331 = c313

    c323 = cotth + 4 * a2 * r * s2th / (a2 + 2 * r2 + a2 * c2th)^2
    c332 = c323

    c333 = (- a * r * sigma2 * sth2 + a3 * sigma_m * sth4) / sigma3

    ---------------------------------------- 

    c = [[[c000,c001,c002,c003], [c010,c011,c012,c013], [c020,c021,c022,c023], [c030,c031,c032,c033]],
         [[c100,c101,   0,c103], [c110,c111,c112,c113], [   0,c121,c122,   0], [c130,c131,   0,c133]],
         [[c200,c201,   0,c203], [c210,c211,c212,c213], [   0,c221,c222,   0], [c230,c231,   0,c233]],
         [[c300,c301,c302,c303], [c310,c311,c312,c313], [c320,c321,c322,c323], [c330,c331,c332,c333]]]

--------------------------------------------------------------------------------
