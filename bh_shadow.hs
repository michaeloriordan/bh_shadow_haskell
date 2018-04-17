import Data.List 
import System.IO
import Geometry as G
import Control.Parallel.Strategies (withStrategy,parListChunk,rseq)

--------------------------------------------------------------------------------

data Coords = Schwarzschild_GP | Kerr_BL | Kerr_KS deriving (Eq)
data Integrator = RK4 deriving (Eq)

coords_error     = error "Unknown coords!"
integrator_error = error "Unknown integrator!"

--------------------------------------------------------------------------------

-- Camera distance and inclination
camera_r = 100
camera_i = (pi / 180) * 75

-- Camera size
cxlims = (-10, 10)
cylims = (-10, 10)

-- Number of pixels (photons)
nx = 1024
ny = 1024

-- Initial k^0 component of photon momentum
k0_init = 10.0

-- Coordinate system
coords = Kerr_KS

-- Black hole spin
spin = 0.9
rh = 1 + sqrt (1 - spin^2)

-- Radius beyond which photon has escaped
rmax = camera_r + 10

-- Integration method
integrator = RK4

-- Stepsize parameter
step_epsilon = 0.01

-- Max number of steps before photon considered stuck
nmax = 100000

-- Stop slightly outside horizon in Schwarzschild or Boyer-Lindquist coords
rmin
    | coords == Kerr_BL = rh + 1.0e-6
    | otherwise         = rh

-- Run code in parallel 
do_parallel = True

-- Divide tasks into chunks
chunk_size = 128

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
init_photons = map $ init_photon k0_init camera_r camera_i

--------------------------------------------------------------------------------

gcov :: [Double] -> [[Double]]
gcov
    | coords == Schwarzschild_GP = G.gcov_schwarzschild_GP
    | coords == Kerr_BL          = G.gcov_kerr_BL spin
    | coords == Kerr_KS          = G.gcov_kerr_KS spin
    | otherwise                  = coords_error

gcon :: [Double] -> [[Double]]
gcon 
    | coords == Schwarzschild_GP = G.gcon_schwarzschild_GP
    | coords == Kerr_BL          = G.gcon_kerr_BL spin
    | coords == Kerr_KS          = G.gcon_kerr_KS spin
    | otherwise                  = coords_error

conn :: [Double] -> [[[Double]]]
conn
    | coords == Schwarzschild_GP = G.conn_schwarzschild_GP
    | coords == Kerr_BL          = G.conn_kerr_BL spin
    | coords == Kerr_KS          = G.conn_kerr_KS spin
    | otherwise                  = coords_error

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
step_geodesic 
    | integrator == RK4 = step_geodesic_rk4
    | otherwise         = integrator_error

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
    | coords == Kerr_KS          = bound_spherical' (photon_x ph) (photon_k ph)
    | otherwise                  = coords_error

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
parmap = parmap' chunk_size

parmap' :: Int -> (a -> b) -> [a] -> [b]
parmap' chunk f = withStrategy (parListChunk chunk rseq) . map f

map' :: (a -> b) -> [a] -> [b]
map' = if do_parallel then parmap else map

--------------------------------------------------------------------------------

propagate_photon' :: Int -> Photon -> Photon
propagate_photon' n ph 
    | photon_finished ph || n > nmax = ph
    | otherwise = propagate_photon' (n+1) $ step_photon ph

propagate_photon :: Photon -> Photon
propagate_photon = propagate_photon' 0

propagate_photons :: [Photon] -> [Photon]
propagate_photons = map' propagate_photon

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
