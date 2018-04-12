data Photon = Photon [Double] [Double] deriving (Show)

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

conn_schwarzschild_GP r th = c where
    b = sqrt (2 / r)
    br = b * r
    sth = sin th
    cth = cos th
    sth2 = sth^2

    r2 = r^2;
    r3 = r^3;

    ----------------------------------------

    c000 = ([0,0,0], b / r2)
    c010 = ([0,1,0], 1 / r2)
    c001 = ([0,0,1], snd c010)
    c011 = ([0,1,1], 1 / (br * r))
    c022 = ([0,2,2], -br)
    c033 = ([0,3,3], -br * sth2)

    ----------------------------------------

    c100 = ([1,0,0], (r - 2) / r3)
    c110 = ([1,1,0], - b / r2)
    c101 = ([1,0,1], snd c110)
    c111 = ([1,1,1], -1 / r2)
    c122 = ([1,2,2], 2 - r)
    c133 = ([1,3,3], -(r - 2) * sth2)

    ---------------------------------------- 

    c221 = ([2,2,1], 1 / r)
    c212 = ([2,1,2], snd c221)
    c233 = ([2,3,3], -cth * sth)

    ----------------------------------------

    c331 = ([3,3,1], 1 / r)
    c313 = ([3,1,3], snd c331)
    c332 = ([3,3,2], cth / sth)
    c323 = ([3,2,3], snd c332)

    ----------------------------------------  

    c = 0
