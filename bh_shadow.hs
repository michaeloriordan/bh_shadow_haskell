data Photon = Photon [Double] [Double] deriving (Show)

x1 = [1.0, 2.0, 3.0, 4.0]
k1 = [1.0, 2.0, 3.0, 4.0]

x2 = [5.0, 6.0, 7.0, 8.0]
k2 = [5.0, 6.0, 7.0, 8.0]

xs = [x1, x2]
ks = [k1, k2]

p = Photon x1 k1

ps1 = map (Photon x1) [k1, k2]

ps2 = map (\x -> Photon x k1) [x1, x2]

ps3 = [Photon x k | (x, k) <- zip xs ks]

cxmax = 5
cxmin = -5
cymax = 5
cymin = -5

nx = 10
ny = 10

dx = (cxmax - cxmin) / nx
dy = (cymax - cymin) / ny

cxs = [cxmin, cxmin+dx .. cxmax]
cys = [cymin, cymin+dy .. cymax]

cpoints = [(x, y) | x <- cxs, y <- cys]

camera_r = 100
camera_i = pi/2

kk0 = 10

init_photon :: Double -> Double -> Double -> Double -> Photon
init_photon cr ci x y = Photon xi ki where
    sini = sin ci
    cosi = cos ci

    r = sqrt $ x^2 + y^2 + cr^2 
    th = acos $ (y*sini + cr*cosi) / r
    phi = atan2 x $ cr*sini - y*cosi
    xi = [0.0, r, th, phi]

    k1 = - (cr / r) * kk0
    k2 = kk0 * (cosi - (y*sini + cr*cosi) * (cr / r^2)) / 
         (sqrt (x^2 + (cr*sini - y*cosi)^2))
    k3 = ((x*sini) / (x^2 + (cr*sini - y*cosi)^2)) * kk0

    eta0 = -1
    eta1 = 1
    eta2 = r^2
    eta3 = r^2 * (sin th)^2

    k0 = sqrt (-(k1*k1*eta1 + k2*k2*eta2 + k3*k3*eta3) / eta0)

    ki = [k0, k1, k2, k3]

photons = [init_photon camera_r camera_i x y | (x, y) <- cpoints]
