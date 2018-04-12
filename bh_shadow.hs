import Data.List 

data Photon = Photon {photon_x, photon_k :: [Double]} deriving (Show)

camera_r = 100
camera_i = pi/2

cxmax = 5
cxmin = -5
cymax = 5
cymin = -5

nx = 10
ny = 10

kk0 = 10.0

dx = (cxmax - cxmin) / nx
dy = (cymax - cymin) / ny

cxs = [cxmin, cxmin+dx .. cxmax]
cys = [cymin, cymin+dy .. cymax]

cpoints = [(x, y) | x <- cxs, y <- cys]

-- Assuming camera far from BH => flat space - Johannsen & Psaltis (2010)
init_photon :: Double -> Double -> Double -> Double -> Double -> Photon
init_photon cr ci x y k0 = Photon xi ki where
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

photons = [init_photon camera_r camera_i x y kk0 | (x, y) <- cpoints]

gcov_schwarzschild_GP :: Double -> Double -> [[Double]]
gcov_schwarzschild_GP r th = g where
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

gcon_schwarzschild_GP :: Double -> Double -> [[Double]]
gcon_schwarzschild_GP r th = g where
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

conn_schwarzschild_GP :: Double -> Double -> [[[Double]]]
conn_schwarzschild_GP r th = c where
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

gcov :: [Double] -> Double -> [[Double]]
gcov (x0:x1:x2:x3) a = g where
    g = gcov_schwarzschild_GP x1 x2

gcon :: [Double] -> Double -> [[Double]]
gcon (x0:x1:x2:x3) a = g where
    g = gcon_schwarzschild_GP x1 x2

conn :: [Double] -> Double -> [[[Double]]]
conn (x0:x1:x2:x3) a = g where
    g = conn_schwarzschild_GP x1 x2

dkdl :: [Double] -> [Double] -> [Double] 
dkdl x k = dk where
    dk = [1, 1, 1, 1]

step_geodesic_rk4 :: Photon -> Double -> Photon
step_geodesic_rk4 ph dl = Photon xp kp where
    x = photon_x ph
    k = photon_k ph

    dll = (dl / 2)

    f1x = k
    f1k = dkdl x k

    kt1 = [ki + dll * fi | (ki, fi) <- zip k f1k]
    xt1 = [xi + dll * fi | (xi, fi) <- zip x f1x]

    f2x = kt1
    f2k = dkdl xt1 kt1

    kt2 = [ki + dll * fi | (ki, fi) <- zip k f2k]
    xt2 = [xi + dll * fi | (xi, fi) <- zip x f2x]

    f3x = kt2
    f3k = dkdl xt2 kt2

    kt3 = [ki + dl * fi | (ki, fi) <- zip k f3k]
    xt3 = [xi + dl * fi | (xi, fi) <- zip x f3x]

    f4x = kt3
    f4k = dkdl xt3 kt3

    frac = 0.166666666666667

    dx = [frac*dl * (f1 + 2*(f2 + f3) + f4) | (f1,f2,f3,f4) <- zip4 f1x f2x f3x f4x]
    dk = [frac*dl * (f1 + 2*(f2 + f3) + f4) | (f1,f2,f3,f4) <- zip4 f1k f2k f3k f4k]

    xp = [xi + dxi | (xi, dxi) <- zip x dx]
    kp = [ki + dki | (ki, dki) <- zip k dk]
