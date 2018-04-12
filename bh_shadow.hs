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
