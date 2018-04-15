import Data.List 
import System.IO
import Control.Parallel.Strategies (Strategy,withStrategy,parListChunk,rseq)

--------------------------------------------------------------------------------

data Coords = Schwarzschild_GP | Kerr_BL deriving (Eq)
data Integrator = RK4 deriving (Eq)

--------------------------------------------------------------------------------

-- Camera distance and inclination
camera_r = 100
camera_i = pi/2

-- Camera size
cxlims = (-10, 10)
cylims = (-10, 10)

-- Number of pixels (photons)
nx = 64
ny = 64

-- Initial k^0 component of photon momentum
k0_init = 10.0

-- Coordinate system
coords = Kerr_BL

-- Black hole spin
spin = 0.9
rh = 1 + sqrt (1 - spin^2)

-- Radius beyond which photons have "escaped"
rmax = camera_r + 10

-- Integration method
integrator = RK4

-- Stepsize parameter
step_epsilon = 0.01

-- Max number of steps before photon "stuck"
nmax = 100000

-- Stop slightly outside horizon in Schwarzschild or Boyer-Lindquist coords
rmin
    | coords == Kerr_BL = rh + 1.0e-6
    | otherwise         = rh

-- Run code in parallel 
do_parallel = False

-- Divide tasks into chunks
chunk_size = round $ (nx * ny) / 8

--------------------------------------------------------------------------------

data Photon = Photon {photon_x, photon_k :: [Double]} deriving (Show)

photon_r :: Photon -> Double
photon_r ph = r where
    (_:r:_) = photon_x ph

photon_th :: Photon -> Double
photon_th ph = th where
    (_:_:th:_) = photon_x ph

photon_phi :: Photon -> Double
photon_phi ph = phi where
    (_:_:_:phi:_) = photon_x ph

photon_pos :: Photon -> (Double, Double, Double)
photon_pos ph = (photon_r ph, photon_th ph, photon_phi ph)

newtype Pixel = Pixel {pixel_xy :: (Double, Double)} deriving (Show)

--------------------------------------------------------------------------------

cxmin = fst cxlims
cxmax = snd cxlims
cymin = fst cylims
cymax = snd cylims

dx = (cxmax - cxmin) / (nx-1)
dy = (cymax - cymin) / (ny-1)

cxs = [cxmin, cxmin+dx .. cxmax]
cys = [cymin, cymin+dy .. cymax]

init_pixels = [Pixel (x, y) | x <- cxs, y <- cys]

--------------------------------------------------------------------------------

-- Assuming camera far from BH => flat space - Johannsen & Psaltis (2010)
init_photon :: Double -> Double -> Double -> Pixel -> Photon
init_photon k0 cr ci pixel = Photon xi ki where
    (x, y) = pixel_xy pixel
    sini = sin ci
    cosi = cos ci

    r = sqrt $ x^2 + y^2 + cr^2 
    th = acos $ (y*sini + cr*cosi) / r
    phi = atan2 x $ cr*sini - y*cosi

    k1 = k0 * (-cr / r)
    k2 = k0 * (cosi - (y*sini + cr*cosi) * (cr / r^2)) / 
         (sqrt $ x^2 + (cr*sini - y*cosi)^2)
    k3 = k0 * (x*sini) / (x^2 + (cr*sini - y*cosi)^2)

    xi = [0.0, r, th, phi]
    ki = [k0, k1, k2, k3]

init_photons :: [Pixel] -> [Photon]
init_photons pixels = map (init_photon k0_init camera_r camera_i) pixels

--------------------------------------------------------------------------------

gcov_schwarzschild_GP :: [Double] -> [[Double]]
gcov_schwarzschild_GP (_:r:th:_) = g where
    r2 = r^2
    b = 1 - (2 / r)
    sth2 = (sin th)^2

    g00 = -b
    g11 = 1
    g22 = r2
    g33 = r2 * sth2
    g01 = sqrt (2 / r)
    g10 = g01

    g = [[g00,g01,0,0], [g10,g11,0,0], [0,0,g22,0], [0,0,0,g33]]

gcon_schwarzschild_GP :: [Double] -> [[Double]]
gcon_schwarzschild_GP (_:r:th:_) = g where
    r2 = r^2
    b = 1 - (2 / r)
    sth2 = (sin th)^2

    g00 = -1
    g11 = b
    g22 = 1 / r2
    g33 = 1 / (r2 * sth2)
    g01 = sqrt (2 / r)
    g10 = g01

    g = [[g00,g01,0,0], [g10,g11,0,0], [0,0,g22,0], [0,0,0,g33]]

conn_schwarzschild_GP :: [Double] -> [[[Double]]]
conn_schwarzschild_GP (_:r:th:_) = c where
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

    c = [[[c000, c001, 0, 0], [c010, c011, 0, 0], [0, 0, c022, 0], [0, 0, 0, c033]],
         [[c100, c101, 0, 0], [c110, c111, 0, 0], [0, 0, c122, 0], [0, 0, 0, c133]],
         [[0, 0, 0, 0], [0, 0, c212, 0], [0, c221, 0, 0], [0, 0, 0, c233]],
         [[0, 0, 0, 0], [0, 0, 0, c313], [0, 0, 0, c323], [0, c331, c332, 0]]]

--------------------------------------------------------------------------------

delta_kerr :: Double -> Double
delta_kerr r = r^2 - 2*r + spin^2;

sigma_kerr :: Double -> Double -> Double
sigma_kerr r th = r^2 + (spin^2) * (cos th)^2

gcov_kerr_BL :: [Double] -> [[Double]]
gcov_kerr_BL (_:r:th:_) = g where
    sigma = sigma_kerr r th
    delta = delta_kerr r
    sth2 = (sin th)^2
    r2 = r^2
    a = spin
    a2 = a^2
    b = 2 * r / sigma

    g00 = -(1 - b)
    g11 = sigma / delta
    g22 = sigma
    g33 = (r2 + a2 + b * a2 * sth2) * sth2
    g03 = - b * a * sth2
    g30 = g03

    g = [[g00,0,0,g03], [0,g11,0,0], [0,0,g22,0], [g30,0,0,g33]]

gcon_kerr_BL :: [Double] -> [[Double]]
gcon_kerr_BL (_:r:th:_) = g where
    sigma = sigma_kerr r th
    delta = delta_kerr r
    sth2 = (sin th)^2
    r2 = r^2
    a = spin
    a2 = a^2

    a_kerr = (r2 + a2) * (r2 + a2) - a2 * delta * sth2

    g00 = -a_kerr / (sigma * delta)
    g11 = delta / sigma
    g22 = 1 / sigma
    g33 = (delta - a2 * sth2) / (sigma * delta * sth2)
    g03 = - 2 * a * r / (delta * sigma)
    g30 = g03

    g = [[g00,0,0,g03], [0,g11,0,0], [0,0,g22,0], [g30,0,0,g33]]

conn_kerr_BL :: [Double] -> [[[Double]]]
conn_kerr_BL (_:r:th:_) = c where
    delta = delta_kerr r
    sigma = sigma_kerr r th

    cth = cos th
    sth = sin th
    sth2 = sth^2
    cth2 = cth^2
    sth3 = sth^3

    cotth = cth / sth

    a = spin
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
    
    c = [[[0,c001,c002,0], [c010,0,0,c013], [c020,0,0,c023], [0,c031,c032,0]],
         [[c100,0,0,c103], [0,c111,c112,0], [0,c121,c122,0], [c130,0,0,c133]],
         [[c200,0,0,c203], [0,c211,c212,0], [0,c221,c222,0], [c230,0,0,c233]],
         [[0,c301,c302,0], [c310,0,0,c313], [c320,0,0,c323], [0,c331,c332,0]]]

--------------------------------------------------------------------------------

gcov :: [Double] -> [[Double]]
gcov x
    | coords == Schwarzschild_GP = gcov_schwarzschild_GP x
    | coords == Kerr_BL          = gcov_kerr_BL x
    | otherwise = error "Unknown coords!"

gcon :: [Double] -> [[Double]]
gcon x 
    | coords == Schwarzschild_GP = gcon_schwarzschild_GP x
    | coords == Kerr_BL          = gcon_kerr_BL x
    | otherwise = error "Unknown coords!"

conn :: [Double] -> [[[Double]]]
conn x
    | coords == Schwarzschild_GP = conn_schwarzschild_GP x
    | coords == Kerr_BL          = conn_kerr_BL x
    | otherwise = error "Unknown coords!"

--------------------------------------------------------------------------------

dot :: Num a => [a] -> [a] -> a
dot x y = sum $ zipWith (*) x y

dot2 :: Num a => [a] -> [a] -> [[a]] -> a
dot2 x y z = dot x $ map (dot y) z 

-- Geodesic equation
dkdl :: [Double] -> [Double] -> [Double] 
dkdl x k = map (negate . dot2 k k) (conn x)
    
--------------------------------------------------------------------------------

stepsize :: [Double] -> [Double] -> Double
stepsize x k
    | coords == Kerr_BL = min (stepsize' x k) (stepsize'' x k)
    | otherwise         = stepsize' x k

stepsize' :: [Double] -> [Double] -> Double
stepsize' (_:x1:_) (_:k1:k2:k3:_) = dl where
    d1 = abs k1 / x1
    d2 = abs k2
    d3 = abs k3
    dl = step_epsilon / (d1 + d2 + d3)

stepsize'' :: [Double] -> [Double] -> Double
stepsize'' (_:x1:_) (_:k1:_) = dl where
    dl = (x1 - rh) / (2 * abs k1)

--------------------------------------------------------------------------------

step_geodesic_rk4 :: Photon -> Double -> Photon
step_geodesic_rk4 ph dl = phf where
    x = photon_x ph
    k = photon_k ph

    f1x = k
    f1k = dkdl x k

    kt1 = [ki + (dl/2) * fi | (ki, fi) <- zip k f1k]
    xt1 = [xi + (dl/2) * fi | (xi, fi) <- zip x f1x]

    f2x = kt1
    f2k = dkdl xt1 kt1

    kt2 = [ki + (dl/2) * fi | (ki, fi) <- zip k f2k]
    xt2 = [xi + (dl/2) * fi | (xi, fi) <- zip x f2x]

    f3x = kt2
    f3k = dkdl xt2 kt2

    kt3 = [ki + dl * fi | (ki, fi) <- zip k f3k]
    xt3 = [xi + dl * fi | (xi, fi) <- zip x f3x]

    f4x = kt3
    f4k = dkdl xt3 kt3

    dx = [(dl/6) * (f1 + 2*(f2 + f3) + f4) | (f1,f2,f3,f4) <- zip4 f1x f2x f3x f4x]
    dk = [(dl/6) * (f1 + 2*(f2 + f3) + f4) | (f1,f2,f3,f4) <- zip4 f1k f2k f3k f4k]

    xp = [xi + dxi | (xi, dxi) <- zip x dx]
    kp = [ki + dki | (ki, dki) <- zip k dk]

    phf = Photon xp kp

step_geodesic :: Photon -> Double -> Photon
step_geodesic ph dl 
    | integrator == RK4 = step_geodesic_rk4 ph dl
    | otherwise = error "Unknown integrator!"

step_photon :: Photon -> Photon
step_photon ph = phf where
    dl = stepsize (photon_x ph) (photon_k ph)
    phh = step_geodesic ph dl
    phf = bound_spherical phh

--------------------------------------------------------------------------------

bound_spherical :: Photon -> Photon
bound_spherical ph 
    | coords == Schwarzschild_GP = bound_spherical' (photon_x ph) (photon_k ph)
    | coords == Kerr_BL          = bound_spherical' (photon_x ph) (photon_k ph)
    | otherwise = error "Unknown coords!"

-- Assumes x2 and x3 usual theta and phi
-- Force theta to stay in the domain [0, pi] - Chan et al. (2013)
bound_spherical' :: [Double] -> [Double] -> Photon
bound_spherical' (x0:x1:x2:x3:_) (k0:k1:k2:k3:_)
    | x2 > pi   = Photon [x0, x1, 2*pi-x2, x3+pi] [k0, k1, -k2, k3]
    | x2 < 0    = Photon [x0, x1, -x2,     x3-pi] [k0, k1, -k2, k3]
    | otherwise = Photon [x0, x1,  x2,     x3]    [k0, k1,  k2, k3]

--------------------------------------------------------------------------------

photon_finished :: Photon -> Bool
photon_finished ph = (photon_escaped ph) || (photon_captured ph)

photon_escaped :: Photon -> Bool
photon_escaped ph = (photon_r ph) > rmax

photon_captured :: Photon -> Bool
photon_captured ph = (photon_r ph) <= rmin

--------------------------------------------------------------------------------

parmap :: (a -> b) -> [a] -> [b]
parmap f = parmap' chunk_size f

parmap' :: Int -> (a -> b) -> [a] -> [b]
parmap' chunk f = withStrategy (parListChunk chunk rseq) . map f

--------------------------------------------------------------------------------

propagate_photon' :: Int -> Photon -> Photon
propagate_photon' n ph 
    | photon_finished ph || n > nmax = ph
    | otherwise = propagate_photon' (n+1) $ step_photon ph

propagate_photon :: Photon -> Photon
propagate_photon ph = propagate_photon' 0 ph

propagate_photons :: [Photon] -> [Photon]
propagate_photons phs 
    | do_parallel = parmap propagate_photon phs
    | otherwise   = map    propagate_photon phs

--------------------------------------------------------------------------------

photon_status :: Photon -> Double
photon_status ph
    | photon_captured ph = 0
    | photon_escaped ph  = 1
    | otherwise          = -1

-- Save: "x y r th phi status" 
-- Initial pixel: (x, y)
-- Final position: (r, th, phi)
-- Status: escaped, captured, or stuck
data_to_save' :: [Photon] -> [Pixel] -> [[Double]]
data_to_save' phs pixels = data2save where
    positions = map photon_pos phs
    status    = map photon_status phs
    pixels'   = map pixel_xy pixels
    data2save = [[x, y, r, th, phi, stat] 
                 | ((x,y), (r,th,phi), stat) <- zip3 pixels' positions status]

data_to_string :: [[Double]] -> [Char]
data_to_string d = unlines [unwords (map show di) | di <- d]

data_to_save :: [Photon] -> [Pixel] -> [Char]
data_to_save phs pixels = data_to_string $ data_to_save' phs pixels

--------------------------------------------------------------------------------

camera_pixels   = init_pixels
initial_photons = init_photons camera_pixels
final_photons   = propagate_photons initial_photons

main = do
    writeFile "data.txt" $ data_to_save final_photons camera_pixels
